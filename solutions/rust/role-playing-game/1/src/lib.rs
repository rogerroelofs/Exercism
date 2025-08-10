pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        match self.health {
            0 => Some(Self {
                    health: 100,
                    level: self.level,
                    mana: if self.level >= 10 { Some(100) } else { None },
                }),
            _ => None,
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(n) if n < mana_cost => 0,
            None => {
                self.health = if self.health >= mana_cost { self.health - mana_cost } else { 0 };
                0
            },
            _ => {
                self.mana = Some(self.mana.unwrap_or_default() - mana_cost);
                mana_cost * 2
            },
        }
    }
}
