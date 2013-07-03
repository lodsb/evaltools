package TreeQuencer

import SessionLogLoader.StringParser

object TimeParser extends StringParser[Long]{

  def process(s: String): Long = {
    s.toLong
  }
}

