#[derive(Debug, PartialEq)]
enum Tag {
    Func(String, usize),
    Ref(usize),
    Str(usize),
}

#[derive(Debug, PartialEq, Default)]
pub struct Heap(Vec<Tag>);
