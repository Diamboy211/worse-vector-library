#pragma once

#include <cstddef>
#include <utility>
#include <functional>
#include <math.h>

template <std::size_t d, typename f = float>
struct vec
{
	f v[d];
	inline constexpr vec(){}
	inline constexpr vec(const f a)
	{
		for_each([&](f &b){ b = a; });
	}
	template <typename...A>
	inline constexpr vec(const f a, const A...b) : v{a, static_cast<f>(b)...}
	{
		static_assert(sizeof...(b) == d-1);
	}
	inline constexpr vec(const vec<d, f> &a)
	{
		inplace_combine(a, [](f &a, const f &b){a=b;});
	}
	inline constexpr vec(const vec<d, f> &&a)
	{
		inplace_combine(a, [](f &a, const f &b){a=b;});
	}
	template <typename T>
	inline constexpr operator vec<d, T>() const
	{
		return map([](const f &a){ return T(a); });
	}
	inline constexpr vec<d, f> &operator=(const vec<d, f> &a)
	{
		inplace_combine(a, [](f &a, const f &b){a=b;});
		return *this;
	}
	inline constexpr vec<d, f> &operator=(const vec<d, f> &&a)
	{
		inplace_combine(a, [](f &a, const f &b){a=b;});
		return *this;
	}
	inline constexpr f& operator[](const std::size_t i){return v[i];}
	inline constexpr const f& operator[](const std::size_t i) const {return v[i];}
	
	// op is a binary functor
	template <typename F, typename g>
	inline constexpr vec<d, std::invoke_result_t<F,f,g>> combine(const vec<d, g> &b, F &&op) const
	{
		return [&]<std::size_t...t>(std::index_sequence<t...>)
		{
			return vec<d, std::invoke_result_t<F,f,g>>{op(v[t], b.v[t]) ...};
		}(std::make_index_sequence<d>());
	}
	// op is a unary functor
	template <typename F>
	inline constexpr vec<d, std::invoke_result_t<F,f>> map(F &&op) const
	{
		return [&]<std::size_t...t>(std::index_sequence<t...>)
		{
			return vec<d, std::invoke_result_t<F,f>>{op(v[t]) ...};
		}(std::make_index_sequence<d>());
	}
	// op is a binary functor
	template <typename F>
	inline constexpr f reduce(F &&op) const
	{
		struct r
		{
			f a;
			F &func;
			r(f a, F &func) : a(a), func(func) {}
			constexpr r operator+(const r &b)
			{
				return r(func(a,b.a),func);
			}
		};
		return [&]<std::size_t...t>(std::index_sequence<t...>)
		{
			return (r(v[t],op) + ...).a;
		}(std::make_index_sequence<d>());
	}
	// op is a unary functor where the argument is an l-value ref
	// op returns void
	template <typename F>
	inline constexpr void for_each(F &&op)
	{
		[&]<std::size_t...t>(std::index_sequence<t...>)
		{
			(op(v[t]), ...);
		}(std::make_index_sequence<d>());
	}
	// op is a binary functor where the first argument is an l-value ref
	// op return void
	template <typename F, typename g>
	inline constexpr void inplace_combine(const vec<d,g> &b, F &&op)
	{
		[&]<std::size_t...t>(std::index_sequence<t...>)
		{
			(op(v[t], b[t]), ...);
		}(std::make_index_sequence<d>());
	}
	inline constexpr vec<d, f> operator+(const vec<d, f> &b) const
	{
		return combine(b, std::plus<f>());
	}
	inline constexpr vec<d, f> operator-(const vec<d, f> &b) const
	{
		return combine(b, std::minus<f>());
	}
	inline constexpr vec<d, f> operator*(const vec<d, f> &b) const
	{
		return combine(b, std::multiplies<f>());
	}
	inline constexpr vec<d, f> operator/(const vec<d, f> &b) const
	{
		return combine(b, std::divides<f>());
	}
	inline constexpr vec<d, f> operator-() const
	{
		return map(std::negate<f>());
	}
	inline constexpr vec<d, f> &operator+=(const vec<d, f> &b)
	{
		inplace_combine(b, [](f &a, const f &b){ a += b; });
		return *this;
	}
	inline constexpr vec<d, f> &operator-=(const vec<d, f> &b)
	{
		inplace_combine(b, [](f &a, const f &b){ a -= b; });
		return *this;
	}
	inline constexpr vec<d, f> &operator*=(const vec<d, f> &b)
	{
		inplace_combine(b, [](f &a, const f &b){ a *= b; });
		return *this;
	}
	inline constexpr vec<d, f> &operator/=(const vec<d, f> &b)
	{
		inplace_combine(b, [](f &a, const f &b){ a /= b; });
		return *this;
	}
	inline constexpr bool operator==(const vec<d, f> &b) const
	{
		return combine(b, std::equal_to<f>()).reduce(std::logical_and<bool>());
	}
	inline constexpr bool operator!=(const vec<d, f> &b) const
	{
		return combine(b, std::not_equal_to<f>()).reduce(std::logical_or<bool>());
	}
	inline constexpr std::size_t size() const { return d; }
};
namespace vecm
{
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> abs(const vec<d, f> &a)
	{
		return a.map([](const f &a){ return std::abs(a); });
	}

	template <std::size_t d, typename f>
	inline constexpr vec<d, f> max(const vec<d, f> &a, const f b)
	{
		return max(a, vec<d, f>(b));
	}
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> max(const f a, const vec<d, f> &b)
	{
		return max(vec<d, f>(a), b);
	}
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> max(const vec<d, f> &a, const vec<d, f> &b)
	{
		return a.combine(b, [](const f &a, const f &b){ return std::max(a, b); });
	}

	template <std::size_t d, typename f>
	inline constexpr vec<d, f> min(const vec<d, f> &a, const f b)
	{
		return min(a, vec<d, f>(b));
	}
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> min(const f a, const vec<d, f> &b)
	{
		return min(vec<d, f>(a), b);
	}
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> min(const vec<d, f> &a, const vec<d, f> &b)
	{
		return a.combine(b, [](const f &a, const f &b){ return std::min(a, b); });
	}

	template <std::size_t...s, std::size_t d, typename f>
	inline constexpr vec<sizeof...(s), f> sw(const vec<d, f> &v)
	{
		return [&]<std::size_t...t>(std::index_sequence<t...>)
		{
			return vec<sizeof...(s), f>(v.v[s] ...);
		}(std::make_index_sequence<sizeof...(s)>());
	}

	template <std::size_t d, std::size_t e, typename f>
	inline constexpr vec<e / d, f> mul_mat_vec(const vec<d, f> &v, const vec<e, f> &m)
	{
		constexpr size_t oc = e / d;
		return [&]<std::size_t...t>(std::index_sequence<t...>)
		{
			vec<oc, f> r(0);
			((r.v[t / d] += v.v[t % d] * m.v[t]), ...);
			return r;
		}(std::make_index_sequence<e>());
	}

	template <std::size_t y, std::size_t x, std::size_t z, typename f>
	inline constexpr vec<x * y, f> mul_mat_mat(const vec<z * y, f> &a, const vec<x * z, f> &b)
	{
		vec<x * y, f> r(0);
		[&]<std::size_t...t>(std::index_sequence<t...>)
		{
			((r[((t / z) % y) * x + (t / (y*z))] += a[(t % z) + ((t/z) % y) * z] * b[((t % z) * x) + t / (y*z)]),...);
		}(std::make_index_sequence<x * y * z>());
		return r;
	}
	template <std::size_t d, typename f>
	inline constexpr vec<d, f> mul_mat_mat_sqr(const vec<d, f> &a, const vec<d, f> &b)
	{
		constexpr std::size_t s = std::sqrt(d);
		static_assert(s*s == d, "not a square matrix");
		return mul_mat_mat<s, s, s, f>(a, b);
	}
	
	template <std::size_t d, typename f>
	inline constexpr f dot(const vec<d, f> &a, const vec<d, f> &b)
	{
		return (a * b).reduce(std::plus<f>());
	}

	template <typename f>
	inline constexpr vec<3, f> cross(const vec<3, f> &a, const vec<3, f> &b)
	{
		return sw<1,2,0>(a) * sw<2,0,1>(b) - sw<2,0,1>(a) * sw<1,2,0>(b);
	}

	template <std::size_t d, typename f>
	inline constexpr f mag2(const vec<d, f> &v)
	{
		return (v * v).reduce(std::plus<f>());
	}
	template <std::size_t d, typename f>
	inline constexpr f mag(const vec<d, f> &v)
	{
		return std::sqrt((v * v).reduce(std::plus<f>()));
	}

	template <std::size_t d, typename f>
	inline constexpr vec<d, f> norm(const vec<d, f> &v)
	{
		f il = 1.0f / std::sqrt((v * v).reduce(std::plus<f>()));
		return v * il;
	}
};
