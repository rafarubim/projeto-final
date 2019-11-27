namespace StoryGenerator
{
  public class StateType
  {
    public string Name { get; private set; }
    public int Arity { get; private set; }
    public StateType(string name, int arity)
    {
      Name = name;
      Arity = arity;
    }

    public override bool Equals(object obj)
    {
      if (base.Equals(obj))
      {
        return true;
      }
      if (obj.GetType() != typeof(StateType))
      {
        return false;
      }
      var state = (StateType)obj;
      return state.Name == Name && state.Arity == Arity;
    }

    public override int GetHashCode()
    {
      return Name.GetHashCode() + Arity.GetHashCode();
    }
  }
}
