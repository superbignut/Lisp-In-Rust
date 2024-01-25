#[derive(Debug)]
struct Mycollection(Vec<i32>);

impl Mycollection {
    fn new() -> Mycollection {
        Mycollection(Vec::new())
    }

    fn add(&mut self, elem: i32) {
        self.0.push(elem);
    }
}

impl FromIterator<i32> for Mycollection {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = i32>,
    {
        let mut c = Mycollection::new();

        for i in iter {
            c.add(i);
        }
        c
    }
}

impl IntoIterator for Mycollection {
    type Item = i32;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        vec![1i32, 2i32].into_iter()
    }
}

pub enum End<V, E> {
    Ok(V),
    Err(E),
}

impl<A, E, V> FromIterator<End<A, E>> for End<V, E>
where
    V: FromIterator<A>,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = End<A, E>>,
    {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::End;
    use crate::Mycollection;

    #[test]
    fn first_test() {
        assert_eq!(1, 2);

        let itter = (0..5).into_iter();
        let c = Mycollection::from_iter(itter);
        assert_eq!(c.0, vec![0, 1, 2, 3, 4]);
    }

    #[test]
    fn second_test() {
        let a: End<i32, u32> = End::Ok(12);
        let b: End<i32, u32> = End::Ok(12);
        let c: End<i32, u32> = End::Ok(12);
        let d = vec![a, b, c];

        let efg: End<Vec<i32>, u32> = End::from_iter(d);
        assert_ne!(1 + 2, 4);
    }
}

//Todo: FromIterator
