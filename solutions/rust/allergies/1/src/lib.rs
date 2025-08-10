use self::Allergen::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u16)]
pub enum Allergen {
    Eggs = 1 << 0,
    Peanuts = 1 << 1,
    Shellfish = 1 << 2,
    Strawberries = 1 << 3,
    Tomatoes = 1 << 4,
    Chocolate = 1 << 5,
    Pollen = 1 << 6,
    Cats = 1 << 7,
}

const ALLERGENS: [Allergen; 8] =
    [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats];

pub struct Allergies {
    allergens: u16,
}

impl Allergies {
    pub fn new(score: u16) -> Self {
        Allergies { allergens: score }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        let allergen: u16 = *allergen as u16;
        self.allergens & allergen == allergen
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        ALLERGENS
            .iter()
            .filter(|a| self.is_allergic_to(a))
            .cloned()
            .collect()
    }
}
