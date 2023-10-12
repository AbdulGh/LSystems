import org.jblas.DoubleMatrix;
import java.util.Scanner;

abstract class Variable {
	public Variable(String symbol, Type type) {
		this.symbol = symbol;
		this.type = type;
	}

	enum Type {
		LINEAR // (n+1)x(n+1) 'homogenised' matrix transformation
	}
	public  Variable.Type getType() {
		return type;
	};

	public abstract DoubleMatrix transform(DoubleMatrix in);

	@Override
	public String toString() {
		return symbol;
	}
//	public abstract DoubleMatrix transform(DoubleMatrix in);

	private String symbol;
	private Type type;
}

class LinearVariable extends Variable {
	//consumes the remainder of the line
	public LinearVariable(String symbol, Scanner s /*, DoubleMatrix transform */) {
		super(symbol, Variable.Type.LINEAR);
		String next = s.next();
		switch (next) {
			case "translation":
				//2d only so far, I hope nextFloat does error checking
				float x = s.nextFloat();
				float y = s.nextFloat();
				transform = new DoubleMatrix(new double[][]{{1, 0, x}, {0, 1, y}, {0, 0, 1}});
				break;
			case "rotation":
				float r = s.nextFloat();
				transform = new DoubleMatrix(new double[][]{{Math.cos(r), -Math.sin(r), 0}, {Math.sin(r), Math.cos(r), 0}, {0, 0, 1}});
				break;
			default:
				throw new IllegalArgumentException("'" + next + "' should be translation or rotation");
		}
		s.nextLine();
	}

	@Override
	public DoubleMatrix transform(DoubleMatrix in) {
		return transform.mmul(in);
	}

	public DoubleMatrix getDoubleMatrix() {
		return transform;
	}

	private final DoubleMatrix transform;
}
