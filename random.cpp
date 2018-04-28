#include <iostream>
#include <bits/stdc++.h>
using namespace std;

ofstream out("random.txt");

int main(){
	out<<"[";
	srand(37376);
	for (int i = 0; i < 500; ++i)
	{	
		out<<(rand()%6 + 1)<<" ,";
	}
	out<<(rand()%6+1)<<" ]\n";
}