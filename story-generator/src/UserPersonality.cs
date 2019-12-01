using System;
using System.Collections.Generic;
using System.Text;

namespace StoryGenerator
{
  public class UserPersonality
  {
    public float Openess { get; private set; }
    public float Conscientiousness { get; private set; }
    public float Extraversion { get; private set; }
    public float Agreeableness { get; private set; }
    public float Neuroticism { get; private set; }

    public UserPersonality(float openess, float conscientiousness, float extraversion, float agreeableness, float neuroticism)
    {
      if (openess < -1 || 1 < openess)
      {
        throw new ArgumentException();
      }
      if (conscientiousness < -1 || 1 < conscientiousness)
      {
        throw new ArgumentException();
      }
      if (extraversion < -1 || 1 < extraversion)
      {
        throw new ArgumentException();
      }
      if (agreeableness < -1 || 1 < agreeableness)
      {
        throw new ArgumentException();
      }
      if (neuroticism < -1 || 1 < neuroticism)
      {
        throw new ArgumentException();
      }
      Openess = openess;
      Conscientiousness = conscientiousness;
      Extraversion = extraversion;
      Agreeableness = agreeableness;
      Neuroticism = neuroticism;
    }
  }
}
