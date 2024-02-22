#include <iostream>
#include <mutex>
#include <string>
#include <thread>

// ERROR: match
thread_local unsigned int rage = 1;

// This should not match
unsigned int another_rage = 1;

// This should not match
unsigned int yet_another_rage;

// This should not match
std::mutex cout_mutex;

void increase_rage(const std::string &thread_name) {
  ++rage; // modifying outside a lock is okay; this is a thread-local variable
  std::lock_guard<std::mutex> lock(cout_mutex);
  std::cout << "Rage counter for " << thread_name << ": " << rage << '\n';
}

int main() {
  std::thread a(increase_rage, "a"), b(increase_rage, "b");

  {
    std::lock_guard<std::mutex> lock(cout_mutex);
    std::cout << "Rage counter for main: " << rage << '\n';
  }

  a.join();
  b.join();
}
