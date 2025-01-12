#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <fstream>

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

std::vector<std::string> read_input_file(std::string filepath){
    std::ifstream f(filepath);
    std::string temp;
    std::vector<std::string> res;
    for (int i = 0;i < 3;i++){
        std::getline(f, temp);
        res.push_back(temp);
    }
    f.close();
    return res;
}

void write_output_file(std::string filepath, std::string res){
    std::ofstream f(filepath, std::ios_base::out);
    f << res;
    f.close();
}

void extended_euclid(BigInt a, BigInt b, BigInt& d, BigInt& x, BigInt& y){
    BigInt q, r;
    std::tie(q, r) = BigInt::div_mod_BigInt(a, b);
    r.trim();
    if (r.getNumber().size() == 0){
        d = b;
        x = BigInt(std::vector<unsigned long>(1, 0), 1);
        y = BigInt(std::vector<unsigned long>(1, 1), 1);
        return;
    }
    extended_euclid(b, r, d, x, y);
    BigInt temp = BigInt::subtractBigInt(x, BigInt::multiplyBigInt(y, q));
    x = y;
    y = temp;
}

std::tuple<bool, BigInt>inverse_mod_BigInt(BigInt a, BigInt mod){
    BigInt gcd, x0, y0;
    extended_euclid(a, mod, gcd, x0, y0);
    if (BigInt::compareBigInt(gcd, BigInt(std::vector<unsigned long>(1, 1), 1)) != 0) return std::make_tuple(0, BigInt(std::vector<unsigned long>(1, 0), 1));
    BigInt k, r;
    std::tie(k, r) = BigInt::div_mod_BigInt(BigInt(x0.getNumber(), !x0.getSign()), mod);
    if (k.getSign()) k = BigInt::subtractBigInt(k, BigInt(std::vector<unsigned long>(1, 1), 0));
    if (k.getNumber().size() == 1 && k.getNumber()[0] == 0 && !k.getSign()) return std::make_tuple(1, x0);
    BigInt x = BigInt::multiplyBigInt(k, mod);
    x.setSign(!x.getSign());
    x = BigInt::subtractBigInt(x0, x);
    return std::make_tuple(1, x);
}

int main(int argc, char* argv[]){
    std::string input_file = argv[1];
    std::string output_file = argv[2];
    std::vector<std::string> input = read_input_file(input_file);
    BigInt p(input[0]);
    BigInt q(input[1]);
    BigInt e(input[2]);
    BigInt phi = BigInt::multiplyBigInt(BigInt::subtractBigInt(p, BigInt(std::vector<unsigned long>(1, 1), 1)), BigInt::subtractBigInt(q, BigInt(std::vector<unsigned long>(1, 1), 1)));    
    BigInt d;
    bool is_coprime;
    std::tie(is_coprime, d) = inverse_mod_BigInt(e, phi);
    std::string res = "-1";
    if (is_coprime){
        res = d.toHexString();
        std::cout << res << std::endl;
    }
    write_output_file(output_file, res);
    return 1;
}   