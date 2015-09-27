template <typename T, int kInt, bool kBool = true>
void foo() {
}

template <>
void foo<float, -7>()
{
}
