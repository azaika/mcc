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

pub fn distinguish(mut x: Id) -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    if let Some(dot) = x.find('.') {
        x.replace_range((dot+1).., &n.to_string());
    }
    else {
        x += &format!(".{}", n);
    }

    x
}