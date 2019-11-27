namespace StoryGenerator
{
  public class Entity
  {
    private string _entity;
    public Entity(string entity)
    {
      _entity = entity;
    }

    public override string ToString()
    {
      return _entity;
    }
  }
}
