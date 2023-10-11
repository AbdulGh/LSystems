import org.jblas.DoubleMatrix;
import java.util.Scanner;

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
	//consumes the remainder of the line
	public LinearVariable(String symbol, Scanner s /*, DoubleMatrix transform */) {
		super(symbol);
		String next = s.next();
		switch (next) {
			case "translation":
				//2d only so far, I hope nextFloat does error checking
				float x = s.nextFloat();
				float y = s.nextFloat();
				transform = new DoubleMatrix(new double[][]{{1, 0, x}, {0, 1, y}});
				break;
			case "rotation":
				float r = s.nextFloat();
				transform = new DoubleMatrix(new double[][]{{Math.cos(r), -Math.sin(r)}, {Math.sin(r), Math.cos(r)}});
				break;
			default:
				throw new IllegalArgumentException("'" + next + "' should be translation or rotation");
		}
		s.nextLine();
	}

	private DoubleMatrix transform;

}
