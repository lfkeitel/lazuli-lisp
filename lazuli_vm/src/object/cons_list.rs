use std::rc::Rc;

#[derive(Clone, Default)]
pub struct ConsList<T: PartialEq> {
    head: Link<T>,
    length: usize,
}

type Link<T> = Option<Rc<Node<T>>>;

struct Node<T: PartialEq> {
    elem: T,
    next: Link<T>,
}

impl<T: PartialEq> ConsList<T> {
    pub fn new() -> Self {
        ConsList {
            head: None,
            length: 0,
        }
    }

    pub fn append(&self, elem: T) -> ConsList<T> {
        ConsList {
            length: self.length + 1,
            head: Some(Rc::new(Node {
                elem,
                next: self.head.clone(),
            })),
        }
    }

    pub fn tail(&self) -> ConsList<T> {
        ConsList {
            head: self.head.as_ref().and_then(|node| node.next.clone()),
            length: if self.length > 0 { self.length - 1 } else { 0 },
        }
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_ref().map(|node| &**node),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        self.length
    }
}

impl<T: PartialEq> Drop for ConsList<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                head = node.next.take();
            } else {
                break;
            }
        }
    }
}

impl<T: PartialEq> PartialEq for ConsList<T> {
    fn eq(&self, other: &ConsList<T>) -> bool {
        if self.length != other.length {
            return false;
        }

        for (i1, i2) in self.iter().zip(other.iter()) {
            if i1 != i2 {
                return false;
            }
        }

        true
    }
}

pub struct Iter<'a, T: PartialEq> {
    next: Option<&'a Node<T>>,
}

impl<'a, T: PartialEq> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_ref().map(|node| &**node);
            &node.elem
        })
    }
}

#[cfg(test)]
mod test {
    use super::ConsList;

    #[test]
    fn basics() {
        let list = ConsList::new();
        assert_eq!(list.head(), None);

        let list = list.append(1).append(2).append(3);
        assert_eq!(list.head(), Some(&3));

        let list = list.tail();
        assert_eq!(list.head(), Some(&2));

        let list = list.tail();
        assert_eq!(list.head(), Some(&1));

        let list = list.tail();
        assert_eq!(list.head(), None);

        // Make sure empty tail works
        let list = list.tail();
        assert_eq!(list.head(), None);
    }

    #[test]
    fn iter() {
        let list = ConsList::new().append(1).append(2).append(3);

        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&1));
    }

    #[test]
    fn test_equality() {
        let list1 = ConsList::new().append(1).append(2).append(3);
        let list2 = ConsList::new().append(1).append(2).append(3);
        let list3 = ConsList::new().append(2).append(1).append(3);

        assert!(list1 == list2);
        assert!(list1 != list3);
    }

    #[test]
    fn test_equality_empty() {
        let list1: ConsList<i32> = ConsList::new();
        let list2: ConsList<i32> = ConsList::new();

        assert!(list1 == list2);
    }
}
