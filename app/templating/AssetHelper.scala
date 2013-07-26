package app
package templating

import play.api.templates.Html
import controllers.routes

trait AssetHelper {

  def cssTag(name: String) = css("stylesheets/" + name)

  private def css(path: String) = Html {
    """<link href="%s?v" type="text/css" rel="stylesheet"/>"""
      .format(routes.Assets.at(path).toString)
  }

}