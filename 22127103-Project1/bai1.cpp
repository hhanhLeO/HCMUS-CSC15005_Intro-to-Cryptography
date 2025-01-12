#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <fstream>
#include <random>

class BigInt{
private:
    const static int bytes_per_chunk = sizeof(unsigned long);
    const static int bits_per_chunk = sizeof(unsigned long) * 8;
    std::vector<unsigned long> _number;
    bool _sign; // 1 - positive ; 0 - negative
public: 
    BigInt(){_sign = 1;}
    BigInt(std::vector<unsigned long> number, bool sign) : _number(number), _sign(sign){}
    BigInt(std::string hex_number){
        int len = hex_number.length();
        for (int i = len;i > 0;i -= bytes_per_chunk * 2){
            int start = std::max(0, i - bytes_per_chunk * 2);
            unsigned long temp = std::stoul(hex_number.substr(start, i - start), nullptr, 16);
            _number.push_back(temp);
        }
        _sign = 1;
    }
    BigInt(std::string hex_number, bool sign) : BigInt(hex_number) { _sign = sign; }
    bool getSign() {return _sign; }
    void setSign(bool sign) {_sign = sign; } 
    std::vector<unsigned long> getNumber(){return _number;}
    void shiftLeft(int shift){
        int len = _number.size();
        int chunk = shift / bits_per_chunk;
        shift %= bits_per_chunk;
        BigInt temp(std::vector<unsigned long>(len + chunk + 1, 0), _sign);
        for (int i = 0;i < len;i++){
            if (shift != 0) temp._number[i + chunk + 1] |= (_number[i] >> (bits_per_chunk - shift));
            temp._number[i + chunk] |= (_number[i] << shift);
        }
        this->_number = temp._number;
        this->trim();
    }
    void shiftRight(int shift){
        int len = _number.size();
        int chunk = shift / bits_per_chunk;
        shift %= bits_per_chunk;
        BigInt temp(std::vector<unsigned long>(len - chunk, 0), _sign);
        for (int i = chunk;i < len;i++){
            if (i - chunk - 1 >= 0 && shift != 0) temp._number[i - chunk - 1] |= (_number[i] << (bits_per_chunk - shift));
            temp._number[i - chunk] |= (_number[i] >> shift);
        }
        this->_number = temp._number;
    }
    void print(){
        for (auto x : _number) std::cout << x << " ";
        std::cout << std::endl;
    }
    std::string toHexString(){
        int len = _number.size();
        if (len == 0) return "";
        std::stringstream builder;
        builder << std::uppercase << std::hex << _number[len - 1];
        for (int i = len - 2;i >= 0;i--){
            builder << std::uppercase << std::hex << std::setfill('0') << std::setw(bits_per_chunk / 4) << _number[i];
        }
        return builder.str();
    }
    void trim(){ while (_number.size() > 0 && _number.back() == 0) _number.pop_back(); }
    int getBitsLength() {
        this->trim();
        int len = _number.size();
        int i = 1;
        while ((_number[len - 1] >> i) > 0 && i < bits_per_chunk) i++;
        return bits_per_chunk * (len - 1) + i;
    }
    bool getIthBit(int i){
        if (i < 0 || i > this->getBitsLength() - 1) throw std::invalid_argument("Out of bits range");
        int jth_chunk = i / bits_per_chunk;
        int offset = i % bits_per_chunk;
        bool res = (this->_number[jth_chunk] >> offset) & 1;
        return res;
    }
public:
    static int compareBigIntWithoutSign(BigInt a, BigInt b){
        a.trim();
        b.trim();
        int len_a = a._number.size();
        int len_b = b._number.size();
        if (len_a < len_b) return -1;
        if (len_a > len_b) return 1;
        for (int i = len_a - 1;i >= 0;i--){
            if (a._number[i] < b._number[i]) return -1;
            if (a._number[i] > b._number[i]) return 1;
        }
        return 0;
    }
    static int compareBigInt(BigInt a, BigInt b){
        if (a._sign != b._sign) return a._sign ? 1 : -1;
        if (a._sign) return compareBigIntWithoutSign(a, b);
        else return compareBigIntWithoutSign(b, a);
    }
    static BigInt subtractBigInt(BigInt a, BigInt b){
        int compare = compareBigInt(a, b);
        if (compare == 0) return BigInt("", 1);
        BigInt res("");
        res._sign = compare == 1 ? 1 : 0;
        if (a._sign == b._sign){
            if ((res._sign && !a._sign) || (!res._sign && a._sign)){
                BigInt temp(a);
                a = b;
                b = temp;
            }
            int len_a = a._number.size();
            int len_b = b._number.size();
            unsigned long borrow = 0;
            for (int i = 0;i < len_a;i++){
                long long diff = (long long)a._number[i] - borrow;
                if (i < len_b) diff -= (long long)b._number[i];
                borrow = (diff >> bits_per_chunk) != 0 ? 1 : 0;
                res._number.push_back(diff & ULONG_MAX);
            }
        }
        else {
            int len_a = a._number.size();
            int len_b = b._number.size();
            int max_len = std::max(len_a, len_b);
            unsigned long long carry = 0;
            for (int i = 0;i < max_len;i++){
                unsigned long long sum = carry;
                if (i < len_a) sum += (unsigned long long)a._number[i];
                if (i < len_b) sum += (unsigned long long)b._number[i];
                carry = (sum >> bits_per_chunk);
                res._number.push_back(sum & ULONG_MAX);
            }
        }
        res.trim();
        return res;
    }
    static BigInt multiplyBigInt(BigInt a, BigInt b){
        a.trim();
        b.trim();
        int len_a = a._number.size();
        int len_b = b._number.size();
        bool sign;
        if (a._sign == b._sign) sign = 1;
        else sign = 0;
        if (len_a == 0 || len_b == 0) return BigInt("", 1);
        if (len_a == 1 && a._number[0] == 1) return BigInt(b._number, sign);
        if (len_b == 1 && b._number[0] == 1) return BigInt(a._number, sign);
        BigInt res(std::vector<unsigned long>(len_a + len_b, 0), 1);
        for (int i = 0;i < len_a;i++){
            unsigned long long carry = 0;
            for (int j = 0;j < len_b || carry;j++){
                unsigned long long product = (unsigned long long)res._number[i + j] + carry;
                if (j < len_b) product += (unsigned long long)a._number[i] * b._number[j];
                res._number[i + j] = product & ULONG_MAX;
                carry = product >> bits_per_chunk;
            }
        }
        res.trim();
        res._sign = sign;
        return res;
    }
    static std::tuple<BigInt, BigInt> div_mod_BigInt(BigInt a, BigInt b){
        int compare = compareBigIntWithoutSign(a, b);
        if (compare == -1) return std::make_tuple(BigInt(std::vector<unsigned long>(1, 0), 1), a);
        if (compare == 0) return std::make_tuple(BigInt(std::vector<unsigned long>(1, 1), a._sign == b._sign ? 1 : 0), BigInt(std::vector<unsigned long>(1, 0), 1));
        a.trim();
        b.trim();
        if (b._number.size() == 0) throw std::invalid_argument("Divided by zero");
        int bits_len_a = a.getBitsLength();
        int bits_len_b = b.getBitsLength();
        BigInt q(std::vector<unsigned long>(a._number.size(), 0), !(a._sign^b._sign));
        bool r_sign = a._sign;
        a._sign = 1;
        b._sign = 1;
        int shift = bits_len_a - bits_len_b;
        b.shiftLeft(shift);
        while (shift >= 0){
            if (compareBigInt(a, b) != -1){
                a = subtractBigInt(a, b);
                q._number[shift / bits_per_chunk] |= ((unsigned long)1 << (shift & 31));
            }
            b.shiftRight(1);
            shift--;
        }
        q.trim();
        a.trim();
        a._sign = r_sign;
        return std::make_tuple(q, a);
    }
    static BigInt modBigInt(BigInt a, BigInt b){
        if (!b._sign) throw std::invalid_argument("Modular number cannot be negative");
        bool a_sign = a._sign;
        a._sign = 1;
        BigInt temp(b);
        int compare = compareBigIntWithoutSign(a, b);
        if (compare == 1){
            a.trim();
            b.trim();
            if (b._number.size() == 0) throw std::invalid_argument("Divided by zero");
            int bits_len_a = a.getBitsLength();
            int bits_len_b = b.getBitsLength();
            int shift = bits_len_a - bits_len_b;
            b.shiftLeft(shift);
            while (shift >= 0){
                if (compareBigIntWithoutSign(a, b) != -1) a = subtractBigInt(a, b);
                b.shiftRight(1);
                shift--;
            }
        }
        else if (compare == 0) return BigInt(std::vector<unsigned long>(1, 0), 1);
        if (!a_sign) a = subtractBigInt(temp, a);
        return a;
    }
    static BigInt powModBigInt(BigInt base, BigInt exponent, BigInt mod){
        if (compareBigInt(exponent, BigInt(std::vector<unsigned long>(1, 0), 1)) == 0) return BigInt(std::vector<unsigned long>(1, 1), 1);
        BigInt res(std::vector<unsigned long>(1, 1), 1);
        BigInt current_base(base);
        int exp_len = exponent.getBitsLength();
        for (int i = 0;i < exp_len;i++){
            if (exponent.getIthBit(i)) res = modBigInt(multiplyBigInt(res, current_base), mod);
            current_base = modBigInt(multiplyBigInt(current_base, current_base), mod);
        }
        return res;
    }
};

std::string read_input_file(std::string filepath){
    std::ifstream f(filepath);
    std::string hex = "";
    std::getline(f, hex);
    f.close();
    return hex;
}

void write_output_file(std::string filepath, bool isPrime){
    std::ofstream f(filepath, std::ios_base::out);
    f << isPrime;
    f.close();
}
// Rabin-Miller: p-1=(2^k)*m with m is odd
bool Rabin_Miller_test(const BigInt& a, const BigInt& p, int k, const BigInt& m){
    BigInt res = BigInt::powModBigInt(a, m, p);
    BigInt p_minus_1 = BigInt::subtractBigInt(p, BigInt(std::vector<unsigned long>(1, 1), 1));
    if (BigInt::compareBigInt(res, BigInt(std::vector<unsigned long>(1, 1), 1)) == 0 || BigInt::compareBigInt(res, p_minus_1) == 0) return true;
    for (int i = 1;i < k;i++){
        res = BigInt::powModBigInt(res, BigInt(std::vector<unsigned long>(1, 2), 1), p);
        if (BigInt::compareBigInt(res, p_minus_1) == 0) return true;
    }
    return false;
}

std::vector<BigInt> gen_bases(int k, BigInt p){
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<unsigned long> dist(0, 0xffffffff);
    std::vector<BigInt> res;
    std::vector<unsigned long>p_number = p.getNumber();
    int len = p_number.size();
    for (int j = 0;j < k;j++){
        std::vector<unsigned long> temp;
        dist = std::uniform_int_distribution<unsigned long>(0, 0xffffffff);
        for (int i = 0;i < len - 1;i++){
            unsigned long chunk = dist(gen);
            temp.push_back(chunk);
        }
        dist = std::uniform_int_distribution<unsigned long>(0, p_number[len - 1] - 1);
        temp.push_back(dist(gen));
        res.push_back(BigInt(temp, 1));
    }
    return res;
}

std::vector<int> fixed_bases = {2, 3, 5, 7, 11, 13, 17, 19, 23};
bool is_prime(BigInt p){
    std::vector<unsigned long>p_number = p.getNumber();
    int len = p_number.size();
    if (len < 1) return false;
    if (p_number[0] == 2) return true;
    if (p_number[0] & 1 == 0) return false;

    BigInt m = BigInt::subtractBigInt(p, BigInt(std::vector<unsigned long>(1, 1), 1));
    int k = 0;
    std::vector<unsigned long>m_number = m.getNumber();
    while ((m_number[0] & 1) == 0){
        k++;
        m.shiftRight(1);
        m_number = m.getNumber();
    }
    for (int i = 0;i < fixed_bases.size();i++){
        if (!Rabin_Miller_test(BigInt(std::vector<unsigned long>(1, fixed_bases[i]), 1), p, k, m)) return false;
    }
    if (len < 3) return true;
    std::vector<BigInt> bases = gen_bases(5, p);
    for (int i = 0;i < bases.size();i++){
        if (!Rabin_Miller_test(bases[i], p, k, m)){
            return false;
        }
    }
    return true;
}

int main(int argc, char* argv[]){
    std::string input_file = argv[1];
    std::string output_file = argv[2];
    std::string hex = read_input_file(input_file);
    BigInt p(hex);
    bool isPrime = is_prime(p);
    write_output_file(output_file, isPrime);
    return 1;
}