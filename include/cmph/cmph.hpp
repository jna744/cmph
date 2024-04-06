#ifndef CMPH_CMPH_HPP
#define CMPH_CMPH_HPP

#include <array>
#include <string_view>
#include <type_traits>
#include <utility>

#define CMPH_FWD(x) static_cast<decltype(x)&&>(x)

namespace cmph
{

namespace detail
{

template <typename... Ts>
struct TypeList
{
};

template <auto...>
struct ValueList
{
};

template <typename T, typename...>
using First = T;

template <typename, typename T, typename...>
using Second = T;

template <typename... Ts>
struct Inherit : Ts...
{
};

template <typename T>
struct Identity
{
  using Type = T;
};

template <template <typename...> class Fn, typename List>
struct ApplyImpl;

template <template <typename...> class Fn, template <typename...> class List, typename... Ts>
struct ApplyImpl<Fn, List<Ts...>>
{
  using Type = Fn<Ts...>;
};

template <template <typename...> class Fn, typename List>
using Apply = typename detail::ApplyImpl<Fn, List>::Type;

template <typename Result, typename... Ts>
struct IsUniqueImpl;

template <typename... Ts>
struct IsUniqueImpl<Inherit<Ts...>> : std::true_type
{
};

template <typename... Rs, typename T, typename... Ts>
struct IsUniqueImpl<Inherit<Rs...>, T, Ts...>
    : std::conditional_t<std::is_base_of_v<Identity<T>, Inherit<Rs...>>,
                         std::false_type,
                         IsUniqueImpl<Inherit<Rs..., Identity<T>>, Ts...>>
{
};

template <typename... Ts>
using IsUnique = detail::IsUniqueImpl<Inherit<>, Ts...>;

template <typename T, typename... Ts>
using IsOneOf = std::bool_constant<(std::is_same_v<T, Ts> || ...)>;

template <template <typename...> class Fn, typename... Args>
struct Defer
{
  using Type = Fn<Args...>;
};

template <typename T, template <typename...> class TemplateFn>
struct IsInstanceOf : std::false_type
{
};

template <template <typename...> class TemplateFn, typename... Ts>
struct IsInstanceOf<TemplateFn<Ts...>, TemplateFn> : std::true_type
{
};

}  // namespace detail

template <std::size_t N>
struct StringConstant
{
  std::array<char, N> data{};

  constexpr StringConstant(char const (&arr)[N])
  {
    for (std::size_t i = 0; i < N; ++i)
      data[i] = arr[i];
  }

  template <std::size_t N2>
  friend inline constexpr bool operator==(StringConstant const& lhs,
                                          StringConstant<N2> const& rhs) noexcept
  {
    return std::string_view(lhs) == std::string_view(rhs);
  }

  template <std::size_t N2>
  friend inline constexpr bool operator!=(StringConstant const& lhs,
                                          StringConstant<N2> const& rhs) noexcept
  {
    return std::string_view(lhs) != std::string_view(rhs);
  }

  constexpr operator std::string_view() const noexcept { return {data.data(), data.size() - 1}; }
};

inline namespace string_constant_literals
{

template <StringConstant String>
constexpr auto operator""_sc() noexcept
{
  return String;
}

}  // namespace string_constant_literals

namespace detail
{

constexpr bool isPrime(std::uint32_t value) noexcept
{
  if (value <= 3)
    return value > 1;
  if (value % 2 == 0 || value % 3 == 0)
    return false;
  std::uint32_t i = 5;
  while (i * i <= value)
  {
    if (value % i == 0 || value % (i + 2) == 0)
    {
      return false;
    }
    i += 6;
  }
  return true;
}

constexpr std::uint32_t getNextPrime(std::uint32_t n) noexcept
{
  while (!isPrime(n))
    ++n;
  return n;
}

template <auto Key_, typename Value>
struct KVP
{
  static constexpr auto Key = Key_;
  Value value;
};

}  // namespace detail

template <typename First, typename Second>
struct Pair
{
  First first;
  Second second;
};

template <StringConstant Key, typename Value>
inline constexpr auto kvp(Value&& value)
{
  return detail::KVP<Key, Value&&>{CMPH_FWD(value)};
}

namespace detail
{

template <auto Key>
struct KVPBuilder
{
  template <typename Value>
  constexpr auto operator()(Value&& v) const noexcept
  {
    return kvp<Key>(CMPH_FWD(v));
  }
};

}  // namespace detail

inline namespace kvp_literals
{

template <StringConstant Key>
constexpr auto operator""_kvp() noexcept
{
  return detail::KVPBuilder<Key>{};
}

}  // namespace kvp_literals

// Jenkins one_at_a_time_hash
// Used as the default hash function for MPH operations.
// https://en.wikipedia.org/wiki/Jenkins_hash_function
struct OAATHash
{
  constexpr std::uint32_t operator()(std::string_view key, std::uint32_t seed) noexcept
  {
    for (unsigned char ch : key)
    {
      seed += ch;
      seed += (seed << 10);
      seed ^= (seed >> 6);
    }
    seed += (seed << 3);
    seed ^= (seed >> 11);
    seed += (seed << 15);
    return seed;
  }
};

struct StoreKeys;
struct CompareKeys;

template <typename Hash_, typename KeyCompare_, typename... Tags>
struct BasicPolicy
{
  static_assert(detail::IsUnique<Tags...>::value);
  using Hash = Hash_;
  using KeyCompare = KeyCompare_;
  static constexpr auto StoreKeys = detail::IsOneOf<cmph::StoreKeys, Tags...>::value;
  static constexpr auto CompareKeys = detail::IsOneOf<cmph::CompareKeys, Tags...>::value;
};

using DefaultPolicy = BasicPolicy<OAATHash, std::equal_to<>, StoreKeys, CompareKeys>;

namespace detail
{

template <typename Policy, std::size_t End, typename KH>
constexpr std::size_t checkIndex(KH, [[maybe_unused]] std::string_view key)
{
  if constexpr (Policy::CompareKeys)
  {
    if (!typename Policy::KeyCompare{}(key, KH::key))
      return End;
  }
  return KH::index;
}

template <typename Policy, std::size_t N, typename... KeyHashes>
struct MPHEvaluator;

template <typename Policy, std::size_t N>
struct MPHEvaluator<Policy, N>
{
  static constexpr std::size_t eval(std::string_view, std::uint32_t) noexcept { return N; }
};

template <typename Policy, std::size_t N, typename KH1>
struct MPHEvaluator<Policy, N, KH1>
{
  static constexpr std::size_t eval(std::string_view key, std::uint32_t hash) noexcept
  {
    switch (hash)
    {
      case KH1::hash:
        return checkIndex<Policy, N>(KH1{}, key);
      default:
        return N;
    }
  }
};

template <typename Policy, std::size_t N, typename KH1, typename KH2>
struct MPHEvaluator<Policy, N, KH1, KH2>
{
  static constexpr std::size_t eval(std::string_view key, std::uint32_t hash) noexcept
  {
    switch (hash)
    {
      case KH1::hash:
        return checkIndex<Policy, N>(KH1{}, key);
      case KH2::hash:
        return checkIndex<Policy, N>(KH2{}, key);
      default:
        return N;
    }
  }
};

template <typename Policy, std::size_t N, typename KH1, typename KH2, typename KH3>
struct MPHEvaluator<Policy, N, KH1, KH2, KH3>
{
  static constexpr std::size_t eval(std::string_view key, std::uint32_t hash) noexcept
  {
    switch (hash)
    {
      case KH1::hash:
        return checkIndex<Policy, N>(KH1{}, key);
      case KH2::hash:
        return checkIndex<Policy, N>(KH2{}, key);
      case KH3::hash:
        return checkIndex<Policy, N>(KH3{}, key);
      default:
        return N;
    }
  }
};

template <typename Policy, std::size_t N, typename KH1, typename KH2, typename KH3, typename KH4>
struct MPHEvaluator<Policy, N, KH1, KH2, KH3, KH4>
{
  static constexpr std::size_t eval(std::string_view key, std::uint32_t hash) noexcept
  {
    switch (hash)
    {
      case KH1::hash:
        return checkIndex<Policy, N>(KH1{}, key);
      case KH2::hash:
        return checkIndex<Policy, N>(KH2{}, key);
      case KH3::hash:
        return checkIndex<Policy, N>(KH3{}, key);
      case KH4::hash:
        return checkIndex<Policy, N>(KH4{}, key);
      default:
        return N;
    }
  }
};

template <typename Policy,
          std::size_t N,
          typename KH1,
          typename KH2,
          typename KH3,
          typename KH4,
          typename KH5,
          typename... KHs>
struct MPHEvaluator<Policy, N, KH1, KH2, KH3, KH4, KH5, KHs...>
{
  static constexpr std::size_t eval(std::string_view key, std::uint32_t hash) noexcept
  {
    switch (hash)
    {
      case KH1::hash:
        return checkIndex<Policy, N>(KH1{}, key);
      case KH2::hash:
        return checkIndex<Policy, N>(KH2{}, key);
      case KH3::hash:
        return checkIndex<Policy, N>(KH3{}, key);
      case KH4::hash:
        return checkIndex<Policy, N>(KH4{}, key);
      case KH5::hash:
        return checkIndex<Policy, N>(KH5{}, key);
      default:
        return MPHEvaluator<Policy, N, KHs...>::eval(key, hash);
    }
  }
};

template <auto Key, auto Hash, auto Index>
struct KeyHash
{
  static constexpr auto key = Key;
  static constexpr auto hash = Hash;
  static constexpr auto index = Index;
};

template <typename Policy, typename Hashes, typename Keys, typename Indexes>
struct MakeEvaluatorImpl;

template <typename Policy, typename... Hashes, auto... Keys, std::size_t... Indexes>
struct MakeEvaluatorImpl<Policy,
                         TypeList<Hashes...>,
                         ValueList<Keys...>,
                         std::index_sequence<Indexes...>>
{
  using Type = MPHEvaluator<Policy, sizeof...(Indexes), KeyHash<Keys, Hashes::value, Indexes>...>;
};

template <typename Policy, typename Hashes, typename Keys, typename Indexes>
using MakeEvaluator = typename MakeEvaluatorImpl<Policy, Hashes, Keys, Indexes>::Type;

template <std::uint32_t V>
using HashT = std::integral_constant<std::uint32_t, V>;

template <typename Hash, auto... Keys>
struct HashKeys
{
  template <typename>
  struct Impl;
  template <typename Seed>
  using Next = typename Impl<HashT<getNextPrime(Seed::value + 1)>>::Type;
  template <typename Seed>
  struct Impl
  {
    using Hashes = TypeList<HashT<Hash{}(Keys, Seed::value)>...>;
    using Type = typename std::conditional_t<Apply<IsUnique, Hashes>::value,
                                             Identity<TypeList<Seed, Hashes>>,
                                             Defer<Next, Seed>>::Type;
  };
  using InitialSeed = HashT<getNextPrime(static_cast<float>(sizeof...(Keys)) * 1.4)>;
  using Type = typename Impl<InitialSeed>::Type;
};

template <typename Policy_>
struct PolicyTraits
{
  using Hash = typename Policy_::Hash;
  template <auto... Keys>
  struct MakeLUT
  {
    using HashResult = typename HashKeys<Hash, Keys...>::Type;
    using Seed = Apply<First, HashResult>;
    using Hashes = Apply<Second, HashResult>;
    using Indexes = std::make_index_sequence<sizeof...(Keys)>;
    using Evaluator = MakeEvaluator<Policy_, Hashes, ValueList<Keys...>, Indexes>;
    struct Type
    {
      using Policy = Policy_;
      static constexpr std::size_t indexOf(std::string_view key) noexcept
      {
        return Evaluator::eval(key, Hash{}(key, Seed::value));
      }
    };
  };
};

struct MapFactory
{
  template <typename T, typename Policy, typename... Ts>
  static constexpr auto create(Ts&&... ts);
};

template <typename T>
struct MappedTypeHelper
{
  using Type = T;
};

template <typename T>
struct MappedTypeHelper<Pair<std::string_view, T>>
{
  using Type = T;
};

}  // namespace detail

template <typename T, std::size_t N, typename LUT>
class Map
{
  using Container = std::array<T, N>;

 public:

  using mapped_type = typename detail::MappedTypeHelper<T>::Type;
  using value_type = typename Container::value_type;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type*;
  using const_pointer = value_type const*;
  using iterator = pointer;
  using const_iterator = const_pointer;

  [[nodiscard]] constexpr bool empty() const noexcept { return false; }
  [[nodiscard]] constexpr std::size_t size() const noexcept { return values_.size(); }

  constexpr iterator begin() noexcept { return values_.begin(); }
  constexpr iterator end() noexcept { return values_.end(); }

  constexpr const_iterator begin() const noexcept { return values_.begin(); }
  constexpr const_iterator end() const noexcept { return values_.end(); }

  constexpr iterator find(std::string_view key) noexcept
  {
    auto index = LUT::indexOf(key);
    return begin() + index;
  }
  constexpr const_iterator find(std::string_view key) const noexcept
  {
    auto index = LUT::indexOf(key);
    return begin() + index;
  }

  [[nodiscard]] constexpr bool contains(std::string_view key) const noexcept
  {
    return find(key) != end();
  }

 private:

  friend detail::MapFactory;

  template <typename... KVPs>
  constexpr Map(KVPs&&... kvps) : values_{CMPH_FWD(kvps)...}
  {
  }

  Container values_;
};

template <typename T, typename Policy = DefaultPolicy, typename... Ts>
inline constexpr auto makeMap(Ts&&... ts)
{
  return detail::MapFactory::create<T, Policy>(CMPH_FWD(ts)...);
}

namespace detail
{

template <typename T, typename Policy, typename... Ts>
constexpr auto MapFactory::create(Ts&&... ts)
{
  constexpr auto N = sizeof...(Ts);

  using Traits = PolicyTraits<Policy>;
  using LUT = typename Traits::template MakeLUT<std::decay_t<Ts>::Key...>::Type;

  if constexpr (Policy::StoreKeys)
  {
    using P = Pair<std::string_view, T>;
    return Map<P, N, LUT>(P{ts.Key, CMPH_FWD(ts).value}...);
  }
  else
  {
    return Map<T, N, LUT>(CMPH_FWD(ts).value...);
  }
}

}  // namespace detail

}  // namespace cmph

#endif  // CMPH_CMPH_HPP
