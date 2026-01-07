use crate::objects::{ObjectId, Objects, ObjectsBuilder};

#[derive(Default)]
pub(crate) struct Simulation {
    pub tick_num: usize,
}

impl Simulation {
    fn tick(&mut self, request: Request) -> Response {
        if request.advance_time {
            self.tick_num = self.tick_num.wrapping_add(1);
        }

        let mut response = Response::default();
        {
            let mut ctx = ObjectsBuilder::default();
            let root = ctx.spawn(|ctx| ctx.fmt("tick_num", format_args!("{}", self.tick_num)));
            response.objects = ctx.build();
            response.root_object = Some(root);
        }
        response
    }
}

#[derive(Default)]
pub struct Request {
    pub advance_time: bool,
}

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub root_object: Option<ObjectId>,
}

pub struct SimulationActor {
    // Communication channels
    sender: std::sync::mpsc::Sender<Request>,
    receiver: std::sync::mpsc::Receiver<Response>,
    // Is stale? Ie, are we ticking without having sent back a response? For how many ticks?
    stale_ticks: usize,
    // "Cached" response
    response: Response,
}

impl SimulationActor {
    pub fn start() -> SimulationActor {
        let (request_tx, request_rx) = std::sync::mpsc::channel();
        let (response_tx, response_rx) = std::sync::mpsc::channel();

        std::thread::spawn(move || {
            // Simulation-side loop
            let mut sim = Simulation::default();
            loop {
                let req = match request_rx.recv() {
                    Ok(x) => x,
                    Err(_) => {
                        return;
                    }
                };

                let response = sim.tick(req);
                let result = response_tx.send(response);
                if result.is_err() {
                    // Main loop terminated, exit
                    return;
                }
            }
        });

        SimulationActor {
            sender: request_tx,
            receiver: response_rx,
            stale_ticks: 0,
            response: Response::default(),
        }
    }

    pub fn tick(&mut self, request: Request) -> &Response {
        // IF we don't have stale ticks, send over the request
        if self.stale_ticks == 0 {
            self.sender.send(request).unwrap();
        }
        let response = self.receiver.try_recv().ok();
        match response {
            Some(response) => {
                // We got a response back! Cache it, reset stale tricks, and send over
                // the request
                self.response = response;
                self.stale_ticks = 0;
            }
            None => {
                // If we got no response back...we must drop this request, and increase stale count
                self.stale_ticks += 1;
            }
        }

        &self.response
    }
}
