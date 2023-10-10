package LSystems;

abstract class Variable {
	public Variable(String symbol) {
		this.symbol = symbol;
	}

	enum Type {
		LINEAR // (n+1)x(n+1) 'homogenised' matrix transformation
	}

	@Override
	public String toString() {
		return symbol;
	}
//	public abstract DoubleMatrix transform(DoubleMatrix in);

	private String symbol;
}

class LinearVariable extends Variable {

	public LinearVariable(String symbol /*, DoubleMatrix transform */) {
		super(symbol);
	}

}
