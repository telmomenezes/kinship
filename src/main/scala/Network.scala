/**
 * 
 */
package com.telmomenezes.kinship;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Telmo Menezes (telmo@telmomenezes.com)
 *
 */
public class Network {
	private Map<Integer, Person> people;
	private int totalPeople;
	private int menCount;
	private int womenCount;
	private int mm;
	private int mw;
	private int wm;
	private int ww;
	
	
	public Network() {
		people = new HashMap<Integer, Person>();
		totalPeople = 0;
		menCount = 0;
		womenCount = 0;
		mm = 0;
		mw = 0;
		wm = 0;
		ww = 0;
	}
	
	public void addPerson(int id, String name, String sex,
			int fatherId, int motherId) {
		Person person = new Person(id, name, sex,
				fatherId, motherId);
		people.put(id, person);
		
		totalPeople++;
		if (sex.equals("H")) {
			menCount++;
		}
		else if (sex.equals("F")) {
			womenCount++;
		}
	}
	
	public void updateDemographicMetrics(){
		Set<Integer> keys = people.keySet();
		for (int k : keys) {
			Person child = people.get(k);
			String childSex = child.getSex();
			int fatherId = child.getFatherId();
			int motherId = child.getMotherId();
			if (fatherId > 0) {
				if (childSex.equals("H")) {
					mm++;
				}
				else if (childSex.equals("F")) {
					mw++;
				}
			}
			if (motherId > 0) {
				if (childSex.equals("H")) {
					wm++;
				}
				else if (childSex.equals("F")) {
					ww++;
				}
			}
		}
	}
	
	private void updateDescendents(int origId, Person targ) {
		int targFatherId = targ.getFatherId();
		int targMotherId = targ.getMotherId();
		
		if (targFatherId > 0) {
			Person father = people.get(targFatherId);
			father.addDescendent(origId);
			updateDescendents(origId, father);
		}
		if (targMotherId > 0) {
			Person mother = people.get(targMotherId);
			mother.addDescendent(origId);
			updateDescendents(origId, mother);
		}
	}
	
	public void updateDescendents(){
		Set<Integer> keys = people.keySet();
		for (int k : keys) {
			Person p = people.get(k);
			updateDescendents(p.getId(), p);
		}
	}
	
	public double getAvgDescendents() {
		double avg = 0;
		Set<Integer> keys = people.keySet();
		for (int k : keys) {
			Person p = people.get(k);
			avg += p.getTotalDesc();
		}
		avg /= totalPeople;
		return avg;
	}
	
	public void load(String filePath) {
		try{
			FileInputStream fstream = new FileInputStream(filePath);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			
			String strLine;
			
			// skip header row
			br.readLine();
			
			// parse rows
			while ((strLine = br.readLine()) != null)   {
				int id = 0;
				String name = "";
				String sex = "";
				int fatherId = 0;
				int motherId = 0;
				
				String[] tokens = strLine.split("\t");
				int row = 0;
				for (String t : tokens) {
					switch (row) {
					case 0:
						id = new Integer(t);
						break;
					case 1:
						name = t;
						break;
					case 2:
						sex = t;
						break;
					case 3:
						fatherId = new Integer(t);
						break;
					case 4:
						motherId = new Integer(t);
						break;
					default:
						break;
					}
					row++;
				}
				
				addPerson(id, name, sex, fatherId, motherId);
			}
			
			updateDemographicMetrics();
			updateDescendents();
			
			in.close();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public String toString() {
		String str = "";
		str += "total people:" + totalPeople + 
				"; men: " + menCount + "; women: " + womenCount + "\n";
		str += "mm: " + mm + "; mw: " + mw
				+ "; wm: " + wm + "; ww: " + ww + "\n";
		return str;
	}
	
	public static void main(String args[]) {
		Network net = new Network();
		net.load("Chimane6.txt");
		System.out.println(net);
		double avgDesc = net.getAvgDescendents();
		System.out.println("avg descendents: " + avgDesc);
	}
}
