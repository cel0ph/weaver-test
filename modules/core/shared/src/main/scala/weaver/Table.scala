package weaver


object Table {

  final case class Cell[V](key: String, value: V)

  def apply[V1](key: String, values: V1*): Seq[Cell[V1]] =
    LazyList.from(values).map(value => Cell(key, value))

  def apply[V1, V2](
      keys: (String, String),
      values: (V1, V2)*
  ): Seq[(Cell[V1], Cell[V2])] = {
    val (k1, k2) = keys
    LazyList.from(values).map {
      case (v1, v2) => (Cell(k1, v1), Cell(k2, v2))
    }
  }

  def apply[V1, V2, V3](
      keys: (String, String, String),
      values: (V1, V2, V3)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3])] = {
    val (k1, k2, k3) = keys
    LazyList.from(values).map {
      case (v1, v2, v3) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3)
        )
    }
  }

  def apply[V1, V2, V3, V4](
      keys: (String, String, String, String),
      values: (V1, V2, V3, V4)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4])] = {
    val (k1, k2, k3, k4) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5](
      keys: (String, String, String, String, String),
      values: (V1, V2, V3, V4, V5)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5])] = {
    val (k1, k2, k3, k4, k5) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5, V6](
      keys: (String, String, String, String, String, String),
      values: (V1, V2, V3, V4, V5, V6)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5], Cell[V6])] = {
    val (k1, k2, k3, k4, k5, k6) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5, v6) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5),
          Cell(k6, v6)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5, V6, V7](
      keys: (String, String, String, String, String, String, String),
      values: (V1, V2, V3, V4, V5, V6, V7)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5], Cell[V6], Cell[V7])] = {
    val (k1, k2, k3, k4, k5, k6, k7) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5, v6, v7) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5),
          Cell(k6, v6),
          Cell(k7, v7)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5, V6, V7, V8](
      keys: (String, String, String, String, String, String, String, String),
      values: (V1, V2, V3, V4, V5, V6, V7, V8)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5], Cell[V6], Cell[V7], Cell[V8])] = {
    val (k1, k2, k3, k4, k5, k6, k7, k8) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5, v6, v7, v8) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5),
          Cell(k6, v6),
          Cell(k7, v7),
          Cell(k8, v8)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5, V6, V7, V8, V9](
      keys: (String, String, String, String, String, String, String, String, String),
      values: (V1, V2, V3, V4, V5, V6, V7, V8, V9)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5], Cell[V6], Cell[V7], Cell[V8], Cell[V9])] = {
    val (k1, k2, k3, k4, k5, k6, k7, k8, k9) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5, v6, v7, v8, v9) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5),
          Cell(k6, v6),
          Cell(k7, v7),
          Cell(k8, v8),
          Cell(k9, v9)
        )
    }
  }

  def apply[V1, V2, V3, V4, V5, V6, V7, V8, V9, V10](
      keys: (String, String, String, String, String, String, String, String, String, String),
      values: (V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)*
  ): Seq[(Cell[V1], Cell[V2], Cell[V3], Cell[V4], Cell[V5], Cell[V6], Cell[V7], Cell[V8], Cell[V9], Cell[V10])] = {
    val (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10) = keys
    LazyList.from(values).map {
      case (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => (
          Cell(k1, v1),
          Cell(k2, v2),
          Cell(k3, v3),
          Cell(k4, v4),
          Cell(k5, v5),
          Cell(k6, v6),
          Cell(k7, v7),
          Cell(k8, v8),
          Cell(k9, v9),
          Cell(k10, v10)
        )
    }
  }

}
