// This stub file contains items that aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut counts = HashMap::new();
    for word in magazine {
        *counts.entry(word).or_insert(0) += 1
    }
    for word in note {
        let entry = counts.entry(word).or_insert(0);
        *entry -= 1;
        if *entry < 0 {
            return false;
        }
    }

    true
}
