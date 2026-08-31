package imdb
import scala.io.Source

case class TitleBasics(tconst: String, titleType: Option[String], primaryTitle: Option[String],
                      originalTitle: Option[String], isAdult: Int, startYear: Option[Int], endYear: Option[Int],
                      runtimeMinutes: Option[Int], genres: Option[List[String]])
case class TitleRatings(tconst: String, averageRating: Float, numVotes: Int)
case class TitleCrew(tconst: String, directors: Option[List[String]], writers: Option[List[String]])
case class NameBasics(nconst: String, primaryName: Option[String], birthYear: Option[Int], deathYear: Option[Int],
                      primaryProfession: Option[List[String]], knownForTitles: Option[List[String]])

object ImdbAnalysis {

  // Hint: use a combination of `ImdbData.titleBasicsPath` and `ImdbData.parseTitleBasics`
  lazy val titleBasicsList: List[TitleBasics] = {
    val source = Source.fromFile(ImdbData.titleBasicsPath)
    val data = source.getLines().toList.map(ImdbData.parseTitleBasics)
    source.close()
    data
  }
 
  // Hint: use a combination of `ImdbData.titleRatingsPath` and `ImdbData.parseTitleRatings`
  lazy val titleRatingsList: List[TitleRatings] = {
    val source = Source.fromFile(ImdbData.titleRatingsPath)
    val data = source.getLines().toList.map(ImdbData.parseTitleRatings)
    source.close()
    data
  }
    
  
  // Hint: use a combination of `ImdbData.titleCrewPath` and `ImdbData.parseTitleCrew`
  lazy val titleCrewList: List[TitleCrew] = {
    val source = Source.fromFile(ImdbData.titleCrewPath)
    val data = source.getLines().toList.map(ImdbData.parseTitleCrew)
    source.close()
    data
  }

  
  // Hint: use a combination of `ImdbData.nameBasicsPath` and `ImdbData.parseNameBasics`
  lazy val nameBasicsList: List[NameBasics] = {
    val source = Source.fromFile(ImdbData.nameBasicsPath)
    val data = source.getLines().toList.map(ImdbData.parseNameBasics)
    source.close()
    data
  }

  def task1(list: List[TitleBasics]): List[(String, Int)] = {
    list
      .flatMap(_.genres.getOrElse(List.empty).toSet)
      .groupBy(identity)
      .mapValues(_.size)
      .toList
      .sortBy(-_._2)
  }

  // Hint: There could be an input list that you do not really need in your implementation.
  def task2(l1: List[TitleBasics], l2: List[TitleCrew], l3: List[NameBasics]): List[(String, Int)] = {
    //filter movies to be released between 2010 and 2022
    val movieSet = l1
        .filter(movie =>
          movie.titleType.contains("movie") &&
          movie.startYear.exists(year => year >= 2010 && year <= 2022)
        )
        .map(_.tconst)
        .toSet

    //associate crew to valid movies
    val result = l3.flatMap { crewMember =>
      val totalMovies = crewMember.knownForTitles match {
        case Some(titles) => titles.count(movieSet.contains)
        case None => 0
      }
      //ensure crew associated w atleast 3 movies
      if (totalMovies >= 3) {
        crewMember.primaryName.map(name => (name, totalMovies))
      } else {
        None
      }
    }
    result
}

  def task3(l1: List[NameBasics],l2: List[TitleBasics] , l3: List[TitleCrew], l4: List[TitleRatings]): List[(String, Float)] = {
   // init maps 
    val ratingsMap = l4.map(rating => rating.tconst -> rating).toMap
    val crewMap = l3.map(crew => crew.tconst -> crew).toMap
    val nameMap = l1.map(name => name.nconst -> name).toMap
    
    //filter movies that received a minimum of 10000 votes
    val movies = l2.filter { title =>
      title.titleType.contains("movie") && ratingsMap.get(title.tconst).exists(_.numVotes >= 10000)
    }

    //get directors from valid movies
    val directorMovies = movies.flatMap { movie =>
      crewMap.get(movie.tconst).toList.flatMap { crew =>
        crew.directors.getOrElse(List.empty).map(director => (director, movie.tconst))
      }
    }

    // make sure at lease 2 movies
    val directorMoviesGrouped = directorMovies
      .groupBy(_._1)
      .filter(_._2.size >= 2)

    // calculate directors ratings
    val directorRatings = directorMoviesGrouped.map { case (directorId, movies) =>
      val weightedSumAndVotes = movies.map { case (_, tconst) =>
        val rating = ratingsMap(tconst)
        (rating.averageRating * rating.numVotes, rating.numVotes)
      }.reduce[(Float, Int)] { case ((sum1, votes1), (sum2, votes2)) =>
        (sum1 + sum2, votes1 + votes2)
      }
      val weightedAverage = weightedSumAndVotes._1 / weightedSumAndVotes._2
      (directorId, weightedAverage)
    }

    //sort in descending order and correct format
    directorRatings.flatMap { case (directorId, avgRating) =>
      nameMap.get(directorId).flatMap(_.primaryName).map(name => (name, avgRating))
    }.toList.sortBy(-_._2)
}

  

  def task4(l1:List[TitleBasics], l2: List[TitleRatings]): List[(Int, List[(String, Float)])] = {
    //init movies and ratings
    val movieSet = l1
      .collect{case movie if movie.titleType == Some("movie") => movie.tconst}
      .toSet
    
    val ratingsMap = l2
      .collect{case movie if movieSet.contains(movie.tconst) => movie.tconst -> movie}
      .toMap

      //filter movies and map decades
     val decadeMoviesMap = l1.flatMap { movie => 
      if (movie.startYear.getOrElse(0) >= 1900 && movie.startYear.getOrElse(0) <= 1999 && movie.titleType.contains("movie")) {
        movie.startYear.map(year => ((year - 1900) / 10, movie))
      } else {
        None
      }
    }.groupBy(_._1)

    // map genres with decadesmap
    val decadeGenresMap = decadeMoviesMap.map{ case (decade, movies) =>
      val genreRatingsMap = movies.flatMap { case (_, movie) =>
        movie.genres.map(_.distinct).getOrElse(List.empty).flatMap(genre =>
          ratingsMap.get(movie.tconst).map(rating => (genre, rating.averageRating.toFloat, rating.numVotes)))
      }.groupBy(_._1)
    (decade, genreRatingsMap)
    }

    // calculate weighted avg of genres and sorting
    decadeGenresMap.map{ case (decade, genreRatingsMap) =>
      val genreAverages = genreRatingsMap.map {
        case (genre, ratings) =>
          val (totalWeightedRating, totalVotes) = ratings.foldLeft((0f, 0)) {
            case ((sumRating, sumVotes), (_, rating, votes)) =>
              (sumRating+rating*votes, sumVotes+votes)
          }
          if (totalVotes == 0) (genre, 0f) else
            (genre, totalWeightedRating/totalVotes)
      }
      .toList
      .sortBy(-_._2)
      .take(5)
      (decade, genreAverages)
    }
    .toList
    .filter(_._2.nonEmpty)
    .sortBy(_._1)
  }

  def main(args: Array[String]) {
    val genres = timed("Task 1", task1(titleBasicsList))
    val crews = timed("Task 2", task2(titleBasicsList, titleCrewList, nameBasicsList))
    val topDirectors = timed("Task 3", task3(nameBasicsList, titleBasicsList, titleCrewList, titleRatingsList))
    val topGenres = timed("Task 4", task4(titleBasicsList, titleRatingsList))

    println("---- Task 1 ----")
    println(genres)
    println("---- Task 2 ----")
    println(crews)
    println("---- Task 3 ----")
    println(topDirectors)
    println("---- Task 4 ----")
    println(topGenres)
    println(timing)
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}