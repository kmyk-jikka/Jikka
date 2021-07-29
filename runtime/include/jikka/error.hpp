#ifndef JIKKA_ERROR_HPP
#define JIKKA_ERROR_HPP
/**
 * @file jikka/error.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <cassert>
#include <iostream>
#include <string>

namespace jikka {

template <class T> inline T error(const std::string &message) {
  std::cerr << message << std::endl;
  assert(false);
}

} // namespace jikka

#endif // JIKKA_ERROR_HPP
