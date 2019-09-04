package home.ovo

import java.time.LocalDate

import scala.collection.immutable
import scala.collection.immutable.Seq

final case class DailySnapshot(date: LocalDate, value: BigDecimal)
final case class MonthlyConsumption(month: Int, value: BigDecimal)

object SmartMeter {

  def aggregate(snapshots: Seq[DailySnapshot]): Seq[MonthlyConsumption] = {
    snapshots
      .groupBy(_.date.getMonthValue)
      .flatMap {
        case (month, snapshotsInMonth) ⇒
          val monthlyAggregate: Option[BigDecimal] = for {
            firstDay ← snapshotsInMonth.headOption
            lastDay ← snapshotsInMonth.lastOption
          } yield (lastDay.value - firstDay.value)

          monthlyAggregate.map(a ⇒ MonthlyConsumption(month, a))
      }
      .toList
      .sortBy(_.month)
  }

  def experiment2(snapshots: Seq[DailySnapshot]) = {

    val firstDays: Seq[DailySnapshot] = snapshots
      .groupBy(_.date.getMonthValue)
      .flatMap {
        case (month, snapshotsInMonth) ⇒
          snapshotsInMonth.headOption
      }
      .toList
      .sortBy(_.date.getDayOfYear)

    val relevant: Seq[DailySnapshot] =
      if (firstDays.lastOption == snapshots.lastOption) firstDays
      else
        firstDays ++ snapshots.lastOption

    relevant.foreach { s ⇒
      println(s"what? $s")
    }

    (0 to relevant.size - 2).map { index ⇒
      val lastDay = relevant(index)
      val diff = relevant(index + 1).value - lastDay.value

      MonthlyConsumption(lastDay.date.getMonthValue, diff)
    }

  }

  def experiment(
      input: Seq[DailySnapshot],
      output: Seq[MonthlyConsumption],
      current: MonthlyConsumption) = {

//    input match {
//      case Seq.empty ⇒ output
//      case first +: rest ⇒
//        if (first.date.getMonth == current.month)
//    }

  }
}
