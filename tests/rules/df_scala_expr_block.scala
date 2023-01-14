// https://linear.app/r2c/issue/PA-947

package controllers

import play.api.mvc._
import javax.inject._
import models.UserModel

@Singleton
class AuthController @Inject()(val controllerComponents: ControllerComponents) extends BaseController{
  
  def test1(input: String) = Action { implicit request: Request[AnyContent] =>
    // ruleid: test
    Ok(input)
  }

  def test2(input: String) = Action {
    // ruleid: test
    Ok(input)
  }

  def test3(input: String) = Action {
    var foo = input;
    // ruleid: test
    Ok(foo)
  }
}