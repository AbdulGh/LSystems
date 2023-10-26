package LSystems.Inference;

import org.jblas.DoubleMatrix;

public class TurtleInference {
	TurtleInference(DoubleMatrix input) { 
		translations = new double[input.columns - 1];
		for (int i = 0; i < input.columns - 1; ++i) {
			translations[i] = Math.sqrt(
				Math.pow(input.get(0, i+1) - input.get(0, i), 2) +
				Math.pow(input.get(1, i+1) - input.get(1, i), 2)
			);
		}

		rotations = new double[input.columns - 2];
		for (int i = 0; i < input.columns - 2; ++i) {
			double x1 = input.get(0, i+1) - input.get(0, i);
			double y1 = input.get(1, i+1) - input.get(1, i);
			double x2 = input.get(0, i+2) - input.get(0, i+1);
			double y2 = input.get(1, i+2) - input.get(1, i+1);
			rotations[i] = vectorAngle(x1, y1, x2, y2);
		}

	}

	private static double vectorAngle(double x1, double y1, double x2, double y2) {
		double inner = x1 * x2 + y1 * y2;
		double absProd = Math.sqrt(
			(x1 * x1 + y1 * y1) * (x2 * x2 + y2 * y2)
		);
		return Math.acos(inner / absProd); //lets hope we dont get a zero vector
	}

	private double[] translations;
	private double[] rotations;
}
