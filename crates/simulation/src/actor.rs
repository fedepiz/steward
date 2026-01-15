use bumpalo::Bump;

use crate::simulation::*;

pub struct SimulationActor {
    // Communication channels
    sender: std::sync::mpsc::Sender<Request>,
    receiver: std::sync::mpsc::Receiver<Response>,
    // Is stale? Ie, are we ticking without having sent back a response? For how many ticks?
    stale_ticks: usize,
    // "Cached" response
    response: Response,
    has_crashed: bool,
}

impl SimulationActor {
    pub fn start() -> SimulationActor {
        let (request_tx, request_rx) = std::sync::mpsc::channel();
        let (response_tx, response_rx) = std::sync::mpsc::channel();

        std::thread::spawn(move || {
            tracing_tracy::client::set_thread_name!("Simulation Thread");
            // Simulation-side loop
            let mut sim = Simulation::default();
            let mut arena = Bump::new();
            loop {
                let _span = tracing::info_span!("Main Actor").entered();
                arena.reset();

                let req = match request_rx.recv() {
                    Ok(x) => x,
                    Err(_) => {
                        return;
                    }
                };

                let response = sim.tick(req, &arena);
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
            has_crashed: false,
        }
    }

    pub fn has_critical_error(&self) -> bool {
        self.has_crashed
    }

    pub fn tick(&mut self, request: Request) -> &Response {
        // IF we don't have stale ticks, send over the request
        if self.stale_ticks == 0 {
            if self.sender.send(request).is_err() {
                self.has_crashed = true;
            }
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
