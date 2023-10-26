package LSystems.Inference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;


public class Clustering {

	/* maybe later...
	
	performs 1d clustering/segmentation using KDE
    public Clustering(ArrayList<Double> input, double sigma, int locality) {
		Collections.sort(input);

		//precompute a kernel
		int kernelWidth = 2 * locality - 1;
		double multiplier = 1 / (Math.sqrt(2 * Math.PI) * sigma);
		double powdenom = 2 * sigma * sigma;
		double[] kernel = new double[kernelWidth];
		//could probably do this faster (it's symmetric) but the kernel width is usually pretty small;
		for (int i = 0; i < kernelWidth; ++i) {
			kernel[i] = multiplier * Math.exp(-Math.pow(kernelWidth / 2 - i, 2) / powdenom);
		}

    }

	public Clustering(ArrayList<Double> input) {
		this(input, 1, 5);
	}*/

	public Clustering(ArrayList<Double> _input, double eps) {
		ArrayList<Double> input = new ArrayList<Double>(_input);

		ArrayList<Integer> ordering = new ArrayList<Integer>(input.size());
		for (int i = 0; i < input.size(); ++i) ordering.add(i);
		ordering.sort((i, j) -> Double.compare(input.get(i), input.get(j)));

		Collections.sort(input);

		ArrayList<Integer> boundaries = new ArrayList<Integer>();
		boundaries.add(-1);
		for (int i = 0; i < input.size() - 1; ++i) {
			if (Math.abs(input.get(i+1) - input.get(i)) > eps) boundaries.add(i);
		}
		boundaries.add(input.size() - 1);

		//find some means
		int leftVal = 0;
		for (int right = 1; right < boundaries.size(); ++right) {
			leftVal = boundaries.get(right - 1) + 1;
			int rightVal = boundaries.get(right);
			double acc = 0;
			for (int j = leftVal; j <= rightVal; ++j) acc += input.get(j);
			acc /= (rightVal - leftVal + 1);
			classes.add(acc);
			for (int j = leftVal; j <= rightVal; ++j) input.set(j, acc);
		}
		discretisedInput = new ArrayList<Double>(input.size());
		for (int i = 0; i < input.size(); ++i) {
			discretisedInput.add(input.get(ordering.get(i)));
		}
	}

	public Clustering(ArrayList<Double> input) { 
		this(input, 0.1);
	}

	public ArrayList<Double> getClasses() {
		return classes;
	}

	public ArrayList<Double> getDiscretisedInput() {
		return discretisedInput;
	}

	private ArrayList<Double> classes = new ArrayList<Double>();
	private ArrayList<Double> discretisedInput;
}

class test { //todo learn JUnit
	public static void main(String[] args) {
		/*Clustering test = new Clustering(new ArrayList<Double>(Arrays.asList(0.1, 0.11, 0.12, 0.23, 0.3)));
		System.out.println(test.getClasses());
		System.out.println(test.getDiscretisedInput());
		*/
		/*
		Clustering test2 = new Clustering(new ArrayList<Double>(Arrays.asList(1.0, 2.0, 1.01, 3.0, 5.0, 2.05, 4.97)));
		System.out.println(test2.getClasses());
		System.out.println(test2.getDiscretisedInput());
		*/

		Clustering test2 = new Clustering(new ArrayList<Double>(Arrays.asList(1.0, 5.01, 2.0, 3.0, 4.99)));
		System.out.println(test2.getClasses());
		System.out.println(test2.getDiscretisedInput());


		//Clustering test3 = new Clustering(new ArrayList<Double>(Arrays.asList(5.0, 4.0, 3.0, 2.0, 1.0)));
		//System.out.println(test3.getClasses());
		//System.out.println(test3.getDiscretisedInput());
	}
}