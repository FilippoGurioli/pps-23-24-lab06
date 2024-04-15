package ex2

object Conference:
    enum Question:
        case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL
    
    trait ConferenceReviewing:
        def loadReview(article: Int)(scores: Map[Question, Int]): Unit
        def loadReview(article: Int)(relevance: Int)(significance: Int)(confidence: Int)(fin: Int): Unit
        def orderedScores(article: Int)(question: Question): Seq[Int]
        def averageFinalScore(article: Int): Double
        def acceptedArticles(): Set[Int]
        def sortedAcceptedArticles(): Seq[(Integer, Double)]
        def averageWeightedFinalScoreMap(): Map[Integer, Double]

    object ConferenceReviewing:
        private class ConferenceReviewingImpl extends ConferenceReviewing:

            val articles: Map[Int, Seq[(Question, Integer)]] = Map()

            override def loadReview(article: Int)(relevance: Int)(significance: Int)(confidence: Int)(fin: Int): Unit = 
                articles + ( //TODO it does not handle the case where the article is already present
                    (article, 
                        (Question.CONFIDENCE, confidence) :: 
                        (Question.FINAL, fin) :: 
                        (Question.RELEVANCE, relevance) :: 
                        (Question.SIGNIFICANCE, significance) :: Nil
                    )
                )

            override def loadReview(article: Int)(scores: Map[Question, Int]): Unit = ???

            override def averageWeightedFinalScoreMap(): Map[Integer, Double] = ???

            override def acceptedArticles(): Set[Int] = ???

            override def orderedScores(article: Int)(question: Question): Seq[Int] = ???

            override def sortedAcceptedArticles(): Seq[(Integer, Double)] = ???

            override def averageFinalScore(article: Int): Double = ???