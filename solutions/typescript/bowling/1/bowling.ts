
export class Bowling {
  private static readonly LAST_FRAME = 9;
  private static readonly MAX_PINS = 10;
  private rolls: number[] = [];
  private currentFrame: number = 0;
  private frameRolls: number[] = [];
  private isGameOver: boolean = false;

  public roll(pins: unknown): void {
    if (this.isGameOver) throw new Error('Cannot roll after game is over');
    if (typeof pins !== 'number' || !Number.isInteger(pins)) throw new Error('Invalid roll');
    if (pins < 0) throw new Error('Negative roll is invalid');
    if (pins > Bowling.MAX_PINS) throw new Error('Pin count exceeds pins on the lane');

    // Validate bonus rolls in the 10th frame
    if (this.currentFrame === Bowling.LAST_FRAME) {
      this.validateBonusRolls(pins);
    }

    // Validate frame pin count except for strikes and 10th frame
    if (this.currentFrame < Bowling.LAST_FRAME && this.frameRolls.length === 1 && this.frameRolls[0] + pins > Bowling.MAX_PINS) {
      throw new Error('Pin count exceeds pins on the lane');
    }

    this.rolls.push(pins);
    this.frameRolls.push(pins);

    // Frame progression
    if (this.currentFrame < Bowling.LAST_FRAME) {
      if (pins === Bowling.MAX_PINS || this.frameRolls.length === 2) {
        this.currentFrame++;
        this.frameRolls = [];
      }
      return;
    }
    // 10th frame logic
    if (this.frameRolls.length === 2) {
      if (this.frameRolls[0] === Bowling.MAX_PINS || this.frameRolls[0] + this.frameRolls[1] === Bowling.MAX_PINS) {
        // Allow up to 3 rolls in 10th frame
        return;
      }
      this.isGameOver = true;
    } else if (this.frameRolls.length === 3) {
      this.isGameOver = true;
    }
  }

  public score(): number {
    if (this.rolls.length === 0) throw new Error('Score cannot be taken until the end of the game');
    if (!this.isGameComplete()) throw new Error('Score cannot be taken until the end of the game');
    let score = 0;
    let rollIndex = 0;
    for (let frame = 0; frame <= Bowling.LAST_FRAME; frame++) {
      if (this.rolls[rollIndex] === Bowling.MAX_PINS) {
        score += Bowling.MAX_PINS + this.strikeBonus(rollIndex);
        rollIndex += 1;
      } else if (this.rolls[rollIndex] + this.rolls[rollIndex + 1] === Bowling.MAX_PINS) {
        score += Bowling.MAX_PINS + this.spareBonus(rollIndex);
        rollIndex += 2;
      } else {
        score += this.rolls[rollIndex] + this.rolls[rollIndex + 1];
        rollIndex += 2;
      }
    }
    return score;
  }

  private strikeBonus(rollIndex: number): number {
    return (this.rolls[rollIndex + 1] ?? 0) + (this.rolls[rollIndex + 2] ?? 0);
  }

  private spareBonus(rollIndex: number): number {
    return this.rolls[rollIndex + 2] ?? 0;
  }

  private isGameComplete(): boolean {
    // 10th frame logic
    let frame = 0;
    let rollIndex = 0;
    while (frame < 10 && rollIndex < this.rolls.length) {
      if (this.rolls[rollIndex] === 10) {
        // Strike
        rollIndex += 1;
      } else {
        rollIndex += 2;
      }
      frame++;
    }
    // If not enough rolls for 10 frames
    if (frame < 10) return false;
    // 10th frame bonus logic
    const tenthStart = this.getFrameStart(9);
    const tenth = this.rolls.slice(tenthStart);
    if (tenth.length < 2) return false;
    if (tenth[0] === 10) {
      if (tenth.length < 3) return false;
    } else if (tenth[0] + (tenth[1] ?? 0) === 10) {
      if (tenth.length < 3) return false;
    }
    return true;
  }

  private getFrameStart(frame: number): number {
    let rollIndex = 0;
    let currentFrame = 0;
    while (currentFrame < frame && rollIndex < this.rolls.length) {
      if (this.rolls[rollIndex] === 10) {
        rollIndex += 1;
      } else {
        rollIndex += 2;
      }
      currentFrame++;
    }
    return rollIndex;
  }

  private validateBonusRolls(pins: number): void {
    const tenthFrame = this.rolls.slice(this.getFrameStart(9));
    if (tenthFrame.length === 1) {
      // First bonus roll after strike or spare
      if (tenthFrame[0] === 10 && pins > 10) {
        throw new Error('Pin count exceeds pins on the lane');
      }
    } else if (tenthFrame.length === 2) {
      if (tenthFrame[0] === 10) {
        // Two bonus rolls after strike
        if (pins > 10) {
          throw new Error('Pin count exceeds pins on the lane');
        }
        if (tenthFrame[1] < 10) {
          // If first bonus is not a strike, second cannot be a strike
          if (pins === 10) {
            throw new Error('Pin count exceeds pins on the lane');
          }
          // Their sum cannot exceed 10
          if (tenthFrame[1] + pins > 10) {
            throw new Error('Pin count exceeds pins on the lane');
          }
        }
      } else if (tenthFrame[0] + tenthFrame[1] === Bowling.MAX_PINS) {
        // Bonus roll after spare
        if (pins > 10) {
          throw new Error('Pin count exceeds pins on the lane');
        }
      }
    }
  }
}
