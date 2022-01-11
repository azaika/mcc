use global_counter::primitive::exact::CounterUsize;

pub type Id = String;

static COUNTER : CounterUsize = CounterUsize::new(0);

pub fn gen_tmp_var() -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    format!("T{}", n)
}

pub fn gen_tmp_var_with(s: &str) -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    format!("T{}{}", s, n)
}

pub fn gen_uniq_with(s: &str) -> Id {
    let n = COUNTER.get();
    COUNTER.inc();

    format!("{}{}", s, n)
}

// x を x.157 のようなユニークな形式に変換する
// 既に x.157 のように . がついている場合は x.157.391 ではなく x.391 に置き換える
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