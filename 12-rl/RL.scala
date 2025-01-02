trait State :
    def isValid : Boolean
    def isTerminal : Boolean
    def step[A](a: A) : Any

trait Quality[S, A, R]: 
    def initialize : Quality[S, A, R]
    def getReward(s: S, a: A) : R
    def bestAction(s: S) : A
    def updatedQ(s: S, a: A) : Quality[S, A, R]

enum Action:
    case Up
    case Down
    case Left
    case Right
        
type Reward = Int
        
def State(x: Int, y: Int): State = new:
    def isValid = x >= 0 && x < 12 && y >= 0 && y < 4
    def isTerminal = x > 0 && y == 0
    def step[Action](a: Action) = 
        val (x1, y1) = (a, x, y) match
            case (Action.Up, x, y) => (x, y+1)
            case (Action.Down, x, y) => (x, y-1)
            case (Action.Left, x, y) => (x-1, y)
            case (Action.Right, x, y) => (x+1, y)
        if !State(x1, y1).isValid then (-1, State(x, y))
        else if State(x1, y1).isTerminal && x1 == 11 then (100, State(x1, y1))
        else if State(x1, y1).isTerminal then (-1000, State(x1, y1))
        else (-1, State(x1, y1))



def Quality(q: Map[State, Map[Action, Reward]]): Quality[State, Action, Reward] = new:
    def initialize = Quality(List.range(0,12*4).foldRight(Map())((x, ac) => ac + 
    (State(x % 12, x/12) -> Map(Action.Up -> 0, Action.Down -> 0, Action.Left -> 0, Action.Right -> 0))))
    def getReward(s: State, a: Action) = q.get(s).get(a)
    def bestAction(s: State) = q.getOrElse(s, Map(Action.Up -> 0)).maxBy((_, r: Reward) => r)._1
    def updatedQ(s: State, a: Action) = Quality(q + (s -> (q.getOrElse(s, Map.empty) + (a -> 0))))
    

@main def run() : Unit = {
     println("idek")
}