package Zad2

class AbstractPolymorphicPair {
  //  Class fields
  type A
  type B
  private var _fst: A = _
  private var _snd: B = _


  //  Accessors
  def fst: A = _fst
  def snd: B = _snd


  //  Mutators
  def fst_=(new_fst: A): Unit = { _fst = new_fst }
  def snd_=(new_snd: B): Unit = { _snd = new_snd }


  override def toString = s"($_fst, $_snd)"
}
