mod things;
use crate::things::*;

fn main() {
    let mut things = Things::default();
    println!("{}", size_of::<Thing>());

    things.init();

    let federico = things.spawn();
    federico.set_name("Federico");
    federico.set_flag(Flag::IsPerson, true);
    let federico = federico.id();

    let tianqi = things.spawn();
    tianqi.set_name("Tianqi");
    tianqi.set_flag(Flag::IsPerson, true);
    let tianqi = tianqi.id();

    let einar = things.spawn();
    einar.set_name("Einar");
    einar.set_flag(Flag::IsPerson, true);
    let einar = einar.id();

    things.add_to_list(List::AtLocation, federico, tianqi);
    things.add_to_list(List::AtLocation, federico, einar);

    for _ in 0..30000 {
        things.spawn();
    }

    let mut time = std::time::Duration::default();
    for _ in 0..100 {
        let start = std::time::Instant::now();
        things.pass(|things, this| {
            if let Some(parent) = this.parent(List::AtLocation).as_valid() {
                let value = (this.var(Var::Dummy) + 1.0).sqrt().sqrt().sqrt();
                this.set_var(Var::Dummy, value);
                let parent = &things[parent];
                // println!(
                //     "{} is the child of {}, with a vaue of {value}",
                //     this.name(),
                //     parent.name(),
                // )
            }
        });
        let end = std::time::Instant::now();
        time += end - start
    }
    println!("{:1.2} us", time.as_micros() / 100);

    let mut time = std::time::Duration::default();
    for _ in 0..100 {
        let start = std::time::Instant::now();
        things.pass_readonly(|things, this| {
            if let Some(parent) = this.parent(List::AtLocation).as_valid() {
                let value = (this.var(Var::Dummy) + 1.0).sqrt().sqrt().sqrt();
                let parent = &things[parent];
                // println!(
                //     "{} is the child of {}, with a vaue of {value}",
                //     this.name(),
                //     parent.name(),
                // )
            }
        });
        let end = std::time::Instant::now();
        time += end - start
    }
    println!("{:1.2} us", time.as_micros() / 100);
}
