using System.Diagnostics;
using Microsoft.AspNetCore.Mvc;

namespace MvcMovie.Controllers;

public class HomeCtrl : Controller
{

    private readonly IFoobar _foobar;

    public HomeCtrl()
    {
        _foobar = getFoobar();
    }

    public string Something1(string value) {
      // MATCH: 
      return _foobar.Find(value);
    }

    public string Something2(string value) {
      IFoobar _foobar2 = getFoobar();
      // MATCH: 
      return _foobar2.Find(value);
    }

}

