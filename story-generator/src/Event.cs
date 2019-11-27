using System.Collections.Generic;
using System.Collections.Immutable;

namespace StoryGenerator
{
  public class Event
  {
    public string Name { get; private set; }

    public int Arity { get; private set; }

    public ImmutableList<StateTerm> Args { get; private set; }

    public float OcurrenceTime { get; private set; }

    public Event(string name, int arity, IEnumerable<StateTerm> args, float ocurrenceTime)
    {
      Name = name;
      Arity = arity;
      Args = args.ToImmutableList();
      OcurrenceTime = ocurrenceTime;
    }
  }
}
