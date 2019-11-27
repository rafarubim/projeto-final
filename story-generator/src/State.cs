using System.Collections.Generic;
using System.Collections.Immutable;

namespace StoryGenerator
{
  public class State
  {
    public StateType Type { get; private set; }
    public ImmutableList<StateTerm> Args { get; private set; }

    public State(StateType type, IEnumerable<StateTerm> args)
    {
      Type = type;
      Args = args.ToImmutableList();
    }
  }
}
