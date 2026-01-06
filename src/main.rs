use macroquad::prelude as mq;

fn main() {
    let conf = mq::Conf {
        window_title: "Steward".to_string(),
        window_width: 1600,
        window_height: 900,
        high_dpi: true,
        ..Default::default()
    };

    macroquad::Window::from_config(conf, amain());
}

async fn amain() {
    egui_macroquad::cfg(|ctx| {
        ctx.set_pixels_per_point(1.6);
    });

    loop {
        if mq::is_key_pressed(mq::KeyCode::Escape) {
            break;
        }

        egui_macroquad::ui(|ctx| {
            egui::Window::new("Hello").show(ctx, |ui| {
                ui.label("Hello, World!");
            });
        });

        mq::clear_background(mq::LIGHTGRAY);
        egui_macroquad::draw();

        mq::next_frame().await;
    }
}
