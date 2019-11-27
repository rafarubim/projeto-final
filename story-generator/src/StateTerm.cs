using System;
using System.Collections.Generic;
using System.Text;

namespace StoryGenerator
{
  public class StateTerm
  {
    public Type Type { get; private set; }
    public object Term { get; private set; }
    public StateTerm(Entity ent)
    {
      Term = ent;
      Type = typeof(Entity);
    }
    public StateTerm(State st)
    {
      Term = st;
      Type = typeof(State);
    }
    public StateTerm(Event ev)
    {
      Term = ev;
      Type = typeof(Event);
    }
    public StateTerm(Scalar scl)
    {
      Term = scl;
      Type = typeof(Scalar);
    }
  }
}
