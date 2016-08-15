export default {
    total: scoreList => {
        return scoreList.reduce((score,acum) => {
            return {
                teamAPoint: score.teamAPoint + acum.teamAPoint,
                teamBPoint: score.teamBPoint + acum.teamBPoint,
            }
        }, {teamAPoint:0,teamBPoint:0})
    },
}
