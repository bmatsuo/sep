package com.github.bmatsuo.sep

case class CSVParseError(line: Int, col: Int, message: Option[String])
