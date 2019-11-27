using System;
using System.Collections.Generic;
using System.Text;

namespace StoryGenerator
{
  public class Scalar
  {
    public Type Type { get; private set; }
    public object Value { get; private set; }
    
    public Scalar(float num)
    {
      Value = num;
      Type = typeof(float);
    }
    public Scalar(string enumValue)
    {
      Value = enumValue;
      Type = typeof(string);
    }
  }
}
