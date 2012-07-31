import dsl.Dsl._

val columnDefinitions = new Columns {
  def columns =
    List(
      /**
       * Put all column definitions here, with each separated by a ',' (except the last)
       */
      create column "Num Locations" having {
        count of ("clinical_study", "location")
      },
      create column "US Locations" having {
        count of ("clinical_study", "location") where ("facility", "address", "country") is ("US"|"United States"|"USA")
      }
    )
}
xmltocsv.Main.runWithColumns(columnDefinitions)