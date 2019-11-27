using System;
using System.Collections.Generic;
using System.Text;

namespace StoryGenerator
{
  public class Trigger
  {
    public string Name { get; private set; }
    public float Time { get; private set; }

    public Trigger(string name, float time)
    {
      Name = name;
      Time = time;
    }
  }
}
