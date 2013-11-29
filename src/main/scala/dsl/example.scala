import dsl.Dsl._

object example extends OutputConfiguration {


  // Create the custom columns, of the form:
  // create column "Column Name" having {
  //   column specification (currently only have count, as below)
  // }
  // separate each "create column name having {...}" with a comma
  create column "US Locations" having {
    count of ("clinical_study", "location") where ("facility", "address", "country") is_not ("United States" | "USA")
  }

  create column "Num References" having {
    count of ("clinical_study", "reference")
  }



  filter columns (

  // Define which columns to include by their title
  // Make sure each column definition is surrounded by (), preceding '||' or '&&'
    (column is ("clinical_study", "location", "facility", "address", "country"))
    // The above will filter out all columns except |clinical_study|location|
    // Use parenthesis to group filters join by '||' or '&&'. They have the same precedence in order of operations
  )
   // change 'false' to 'true' to compress the CSV after running.
   // This may cause errors if the file size is too large
  compress = true

  xmltocsv.Main.run_with(this)
}
