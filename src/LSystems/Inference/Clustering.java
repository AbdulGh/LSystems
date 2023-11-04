package LSystems.Inference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.function.Function;
import java.util.stream.IntStream;

public class Clustering {

	/* KDE maybe later...
	
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

	public Clustering(double[] _input, double eps) {
		double[] input = _input.clone();

		//ordering gives you, in the ith coordinate, the original index of the ith element in the sorted array
		//postitionMapping gives you, in the ith coordinate, the position that the ith element lands
		Integer[] ordering = new Integer[input.length];
		for (int i = 0; i < input.length; ++i) ordering[i] = i;
		Arrays.sort(ordering, Comparator.comparingDouble(i -> input[i]));
		int[] positionMapping = new int[input.length];
		for (int i = 0; i < input.length; i++) {
			positionMapping[ordering[i]] = i;
		}

		Arrays.sort(input);
		ArrayList<Integer> boundaries = new ArrayList<Integer>();
		boundaries.add(-1);
		for (int i = 0; i < input.length - 1; ++i) {
			if (Math.abs(input[i+1] - input[i]) > eps) boundaries.add(i);
		}
		boundaries.add(input.length - 1);

		//find some means
		int leftVal = 0;
		for (int right = 1; right < boundaries.size(); ++right) {
			leftVal = boundaries.get(right - 1) + 1;
			int rightVal = boundaries.get(right);
			double acc = 0;
			for (int j = leftVal; j <= rightVal; ++j) acc += input[j];
			acc /= (rightVal - leftVal + 1);
			classes.add(acc);
			for (int j = leftVal; j <= rightVal; ++j) input[j] = acc;
		}

		discretisedInput = new ArrayList<Double>(input.length);
		for (int i = 0; i < input.length; ++i) {
			discretisedInput.add(input[positionMapping[i]]);
		}
	}

	public Clustering(double[] input) { 
		this(input, 0.1);
	}

	public ArrayList<Double> getClasses() {
		return classes;
	}

	public ArrayList<Double> getDiscretisedInput() {
		return discretisedInput;
	}

	public HashMap<Double, String> nameBins(String prefix) {
		HashMap<Double, String> names = new HashMap<>();
		for (int i = 0; i < classes.size(); ++i) {
			names.put(classes.get(i), prefix + Integer.toString(i, 36));
		}
		return names;
	}
	public HashMap<Double, String> nameBins() {
		return this.nameBins("");
	}

	private ArrayList<Double> classes = new ArrayList<Double>();
	private ArrayList<Double> discretisedInput;
}

class test { //todo learn JUnit
	public static void main(String[] args) {
		Clustering test = new Clustering(new double[]{0.1, 0.11, 0.12, 0.23, 0.3});
		System.out.println(test.getClasses());
		System.out.println(test.getDiscretisedInput());

		Clustering test2 = new Clustering(new double[]{1.0, 2.0, 1.01, 3.0, 5.0, 2.05, 4.97});
		System.out.println(test2.getClasses());
		System.out.println(test2.getDiscretisedInput());
	}
}