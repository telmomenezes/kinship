/**
 * 
 */
package com.telmomenezes.kinship;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Telmo Menezes (telmo@telmomenezes.com)
 *
 */
public class Person {
	private int id;
	private String name;
	private String sex;
	private int fatherId;
	private int motherId;
	
	private Set<Integer> descendents;
	private int totalDesc;
	
	public Person(int id, String name, String sex,
			int fatherId, int motherId) {
		this.id = id;
		this.name = name;
		this.sex = sex;
		this.fatherId = fatherId;
		this.motherId = motherId;
		
		descendents = new HashSet<Integer>();
		totalDesc = 0;
	}
	
	public void addDescendent(int descId) {
		descendents.add(descId);
		totalDesc++;
	}
	
	public boolean isDescendent(int descId) {
		return descendents.contains(descId);
	}
	
	public String toString() {
		String str = "name: " + name + "; sex: " + sex
				+ "; father: " + fatherId + "; mother: " + motherId;
		return str;
	}
	
	public int getId() {
		return id;
	}
	
	public String getName() {
		return name;
	}
	
	public String getSex() {
		return sex;
	}

	public int getFatherId() {
		return fatherId;
	}

	public void setFatherId(int fatherId) {
		this.fatherId = fatherId;
	}

	public int getMotherId() {
		return motherId;
	}

	public void setMotherId(int motherId) {
		this.motherId = motherId;
	}

	public int getTotalDesc() {
		return totalDesc;
	}
}