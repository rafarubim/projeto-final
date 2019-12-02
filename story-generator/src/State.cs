using System.Collections.Generic;
using System.Collections.Immutable;

namespace StoryGenerator
{
  public class State
  {
    private StateType _Type { get; set; }

    public string Name
    {
      get
      {
        return _Type.Name;
      }
    }

    public int Arity
    {
      get
      {
        return _Type.Arity;
      }
    }

    public ImmutableList<StateTerm> Args { get; private set; }

    public State(string name, int arity, IEnumerable<StateTerm> args)
    {
      _Type = new StateType(name, arity);
      Args = args.ToImmutableList();
    }
  }
}
