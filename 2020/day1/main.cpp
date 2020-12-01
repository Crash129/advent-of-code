#include <fstream>
#include <iostream>
#include <vector>
#include <chrono>

using namespace std;

#define MIN_DIGIT_COUNT 3
#define MAX_DIGIT_COUNT 4

vector<int> readInput() 
{
    vector<int> result;

    ifstream f("./input", ios::in | ios::binary | ios::ate);
    if (!f.is_open()) {
        return result;
    }

    char* buf = new char[MAX_DIGIT_COUNT + 1];
    result.reserve(f.tellg() / (MIN_DIGIT_COUNT + 1));
    f.seekg(0, ios::beg);
    
    while (f.getline(buf, MAX_DIGIT_COUNT + 1)) {
        result.push_back(atoi(buf));
    }

    delete[] buf;
    f.close();
    
    return result;
}

int step1() 
{
    vector<int> numbers = readInput();
    if (numbers.size() == 0) {
        cout << "Input file could not be opened or is empty" << endl;
        return 0;
    }

    size_t len = numbers.size();
    for (size_t i = 0; i < len; i++) {
        int x = 2020 - numbers.at(i);

        for (size_t j = i; j < len; j++) {
            if (numbers.at(j) == x) {
                return numbers.at(i) * numbers.at(j);
            }
        }
    }

    return 0;
}

int step2()
{
    vector<int> numbers = readInput();
    if (numbers.size() == 0) {
        cout << "Input file could not be opened or is empty" << endl;
        return 0;
    }

    size_t len = numbers.size();
    for (size_t i = 0; i < len; i++) {
        int x = 2020 - numbers.at(i);

        for (size_t j = i; j < len; j++) {
            int xy = x - numbers.at(j);
            if (xy < 0) {
                continue;
            }

            for (size_t k = j; k < len; k++) {
                if (numbers.at(k) == xy) {
                    return numbers.at(i) * numbers.at(j) * numbers.at(k);
                }
            }
        }
    }

    return 0;
}

int main() 
{
    auto start = chrono::high_resolution_clock::now();
    int s1 = step1();
    auto elapsed = chrono::high_resolution_clock::now() - start;
    long long microseconds = chrono::duration_cast<std::chrono::microseconds>(elapsed).count();
    cout << "step1: " << s1 << " (" << microseconds << "µs)" << endl;

    start = chrono::high_resolution_clock::now();
    int s2 = step2();
    elapsed = chrono::high_resolution_clock::now() - start;
    microseconds = chrono::duration_cast<std::chrono::microseconds>(elapsed).count();
    cout << "step2: " << s2 << " (" << microseconds << "µs)" << endl;

    return 0;
}