export class Tournament {
  
  public tally(input: string): string {
    const lines = input.split('\n');
    const teams: { [key: string]: { points: number; matches: number, wins: number, losses: number, draws: number } } = {};

    for (const line of lines) {
      const [team1, team2, outcome] = line.split(';');
      if (!team1 || !team2 || !outcome) {
        continue; // Skip invalid lines
      }
      const points1 = this.getPoints(outcome === 'win', outcome === 'loss');
      const points2 = this.getPoints(outcome === 'loss', outcome === 'win');

      teams[team1] = teams[team1] || { points: 0, matches: 0, wins: 0, losses: 0, draws: 0 };
      teams[team2] = teams[team2] || { points: 0, matches: 0, wins: 0, losses: 0, draws: 0 };

      teams[team1].points += points1;
      teams[team2].points += points2;
      teams[team1].matches += 1;
      teams[team2].matches += 1;

      teams[team1].wins += points1 === 3 ? 1 : 0;
      teams[team2].wins += points2 === 3 ? 1 : 0;
      teams[team1].losses += points1 === 0 ? 1 : 0;
      teams[team2].losses += points2 === 0 ? 1 : 0;
      teams[team1].draws += points1 === 1 ? 1 : 0;
      teams[team2].draws += points2 === 1 ? 1 : 0;
    }

    console.log(teams);

    const sortedTeams = Object.entries(teams)
      .sort((a, b) => b[1].points - a[1].points || a[0].localeCompare(b[0]));

    let results = sortedTeams.map(([team, { matches, points }]) => {
      const wins = teams[team].wins || 0;
      const draws = teams[team].draws || 0;
      const losses = matches - wins - draws;
      return `${team.padEnd(30)} | ${matches.toString().padStart(2)} | ${wins.toFixed(0).padStart(2)} | ${draws.toString().padStart(2)} | ${losses.toString().padStart(2)} | ${points.toString().padStart(2)  }`;
    });
    results.unshift('Team                           | MP |  W |  D |  L |  P');

    return results.join('\n');
  }

  private getPoints(win: boolean, loss: boolean): number {
    if (win) return 3;
    if (loss) return 0;
    return 1; // For a draw
  }
}
