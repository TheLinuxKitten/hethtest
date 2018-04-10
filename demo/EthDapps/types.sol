pragma solidity ^0.4.15;

contract Types {

  address owner;
  mapping (address => int) public valores;

  event NewAnon1(string log, address indexed o) anonymous;
  event NewAnon2(string log, bool a) anonymous;
  event NewAnon3(address indexed o) anonymous;
  event NewAnon4(string indexed o, bool b, uint c) anonymous;
  event New(string log, address indexed o);
  event Valor(string log, address indexed receiver, int48 valor, bool chachi);
  event Enviado(string log, address indexed from, address indexed to, int16 valor, bool chachi);

  function Types() {
    owner = msg.sender;
    NewAnon1("TypesAnon", owner);
    New("Types", owner);
  }

  function func1(address receiver, int32 v) {
    if (msg.sender != owner) return;
    int r = v*9;
    valores[receiver] += r;
    Valor("func1 bool true", receiver, int48(r), true);
    NewAnon1("func1 uint", receiver);
    NewAnon2("func1 int", true);
    NewAnon4("func1 int", true, 345);
  }

  function func2(address receiver, int32 amount) {
    if (valores[msg.sender] < amount) return;
    valores[msg.sender] -= amount;
    valores[receiver] += amount;
    Valor("func2 caca", receiver, amount+10, false);
    Enviado("func2 pedo", msg.sender, receiver, int16(amount), true);
    NewAnon1("func2 pis", receiver);
    NewAnon3(msg.sender);
    NewAnon4("func2 int", false, 123);
  }

  function func3(int32 x, bool y, int8 z) returns (bool r, string s, address t) {
    r = (x-z) > 32 || y;
    s = "pedro";
    t = msg.sender;
  }

  function func4(uint64 a, bool[3] b, int40 c, string d, address e, 
                 bytes f, bool g, bytes3[] h) returns
                (uint64 ra, bool[3] rb, int40 rc, string rd, address re, 
                 bytes rf, bool rg, bytes3[] rh) {
      ra = a;
      rb = b;
      rc = c;
      rd = d;
      re = e;
      rf = f;
      rg = g;
      rh = h;
  }

  function func5(int8 i, uint8 j) returns (int8 ri, uint8 rj, int8 rk, uint8 rl) {
      ri = i;
      rj = j;
      rk = -1;
      rl = 0xff;
  }
}

