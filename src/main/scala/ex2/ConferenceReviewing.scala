package ex2

import scala.collection.immutable.{ Map => ImmutableMap } 
import scala.annotation.meta.setter

object Conference:
    enum Question:
        case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL
    
    trait ConferenceReviewing:
        def loadReview(article: Int)(scores: ImmutableMap[Question, Int]): Unit
        def loadReview(article: Int)(relevance: Int)(significance: Int)(confidence: Int)(fin: Int): Unit
        def orderedScores(article: Int)(question: Question): Seq[Int]
        def averageFinalScore(article: Int): Double
        def acceptedArticles(): Set[Int]
        def sortedAcceptedArticles(): Seq[(Int, Double)]
        def averageWeightedFinalScoreMap(): ImmutableMap[Int, Double]

    object ConferenceReviewing:
        private class ConferenceReviewingImpl extends ConferenceReviewing:

            var articles: ImmutableMap[Int, Seq[(Question, Int)]] = ImmutableMap()

            override def loadReview(article: Int)(relevance: Int)(significance: Int)(confidence: Int)(fin: Int): Unit = 
                articles = articles + ((article, 
                    (articles.get(article) match
                        case Some(seq) => seq
                        case None => Nil).concat(
                        (Question.CONFIDENCE, confidence) :: 
                        (Question.FINAL, fin) :: 
                        (Question.RELEVANCE, relevance) :: 
                        (Question.SIGNIFICANCE, significance) :: Nil
                    )
                ))

            override def loadReview(article: Int)(scores: ImmutableMap[Question, Int]): Unit = 
                loadReview(article)(scores.get(Question.RELEVANCE).get)(scores.get(Question.SIGNIFICANCE).get)(scores.get(Question.CONFIDENCE).get)(scores.get(Question.FINAL).get)

            override def averageWeightedFinalScoreMap(): ImmutableMap[Int, Double] = 
                articles.map((article, scores) => (article, flatMapAndSum(scores)(Question.FINAL) * flatMapAndSum(scores)(Question.CONFIDENCE) / 10))

            override def acceptedArticles(): Set[Int] = articles.flatMap((a, s) => if this.averageFinalScore(a) > 5 then Some(a) else None).toSet

            override def orderedScores(article: Int)(question: Question): Seq[Int] = findFilterMap(article)((q, _) => q == question).sortWith(_ < _)

            override def sortedAcceptedArticles(): Seq[(Int, Double)] = ???

            override def averageFinalScore(article: Int): Double = 
                val scores = findFilterMap(article)((q, _) => q == Question.FINAL)
                scores.sum(Numeric[Int]) / scores.length

            private def flatMapAndSum(seq: Seq[(Question, Int)])(question: Question): Double =
                seq.flatMap(s => if s._1 == Question.FINAL then Some(s._2) else None).sum(using Numeric[Int])


            private def findFilterMap(article: Int)(predicate: ((Question, Int)) => Boolean): Seq[Int] = articles
                .find((a, _) => a == article)
                .get._2
                .filter(predicate)
                .map((q, s) => s)
                