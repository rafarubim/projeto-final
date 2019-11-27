using System.Collections.Generic;
using System.Collections.Immutable;

namespace StoryGenerator
{
  public class State
  {
    private StateType Type { get; set; }

    public string Name
    {
      get
      {
        return Type.Name;
      }
    }

    public int Arity
    {
      get
      {
        return Type.Arity;
      }
    }

    public ImmutableList<StateTerm> Args { get; private set; }

    public State(string name, int arity, IEnumerable<StateTerm> args)
    {
      Type = new StateType(name, arity);
      Args = args.ToImmutableList();
    }
  }
}
