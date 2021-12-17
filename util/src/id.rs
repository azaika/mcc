use global_counter::primitive::exact::CounterUsize;

pub type Id = String;

static COUNTER : CounterUsize = CounterUsize::new(0);

pub fn gen_uniq() -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    format!("T{}", n)
}

pub fn gen_uniq_with(s: &str) -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    format!("T{}{}", s, n)
}