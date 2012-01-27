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

/**
 * @author Telmo Menezes (telmo@telmomenezes.com)
 *
 */
public class Network {
	private Map<Integer, Person> people;
	
	public Network() {
		people = new HashMap<Integer, Person>();
	}
	
	public void load(String filePath) {
		try{
			FileInputStream fstream = new FileInputStream(filePath);
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			
			String strLine;
			
			// skip header row
			strLine = br.readLine();
			
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
					}
					
					Person person = new Person(id, name, sex,
							fatherId, motherId);
					people.put(id, person);
					
					row++;
				}
			}
			
			in.close();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String args[]) {
		Network net = new Network();
		net.load("Chimane6.txt");
		System.out.println("done.");
	}
}
