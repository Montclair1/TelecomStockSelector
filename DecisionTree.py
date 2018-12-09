# Decision Tree Program - Census Data

#Age:  
#0-29: 1            30+: 2

ageInput=int(input("Please enter age:  "))
if ageInput<30:
	age="1"
else:
	age="2"

# Marital Status
# Options:   divorced, married, never married, Separated, Widowed
MS = input("Enter marital status: ")
if (MS!="divorced")and(MS!="married"):
	MS="other"

# Education
# Inputs:  No HS, HS, Associate, Bachelor, Masters, Prof, Doctorate
ed=input("Enter education level: ")

# Hours worked per week
# 0-40: standard    41+:  extra
hoursInput=int(input("enter number of hours worked per week: "))
if hoursInput>40:
	hours="extra"
else:
	hours="standard"
	
# Occupation:  transport, tech, unknown, clerical, craft, armed forces
OccInput=input("enter occupation: ")
if (OccInput=="tech" or OccInput=="unknown"):
	occ="2"
elif (OccInput=="clerical" or OccInput=="craft" or OccInput=="armed forces"):
	occ="1"
else:
	occ="0"
	
# Gender: M F
gender = input("enter gender: ")
	

# our classification output = income level	
# Initialize as unknown in case the tree doesnt cover output
income = "unknown"
	
if (MS!="other"): # left branch
	if (ed!="HS" or ed!="no HS"):
		if MS=="married":
			if hours=="extra":  # applies to all branches
				income="greater"
			else:
				if age == "2":
					if occ == "2":
						income="less"
					else:
						income="greater"
				else:
					income="less"
		if MS=="divorced":
			if hours=="extra": #right tree
				if (ed=="Masters" or ed=="Prof" or ed=="Doctorate"):
					if (occ =="1"):	
						income="greater"
					else:
						income="less"
				else:
					income="less"
	else:
		income="less"
else: # right branch	
	if (ed=="HS" or ed=="no HS"):
		income="less" #left branch)
	else:
		if age=="0":
			income="less" # left branch
		else:
			if hours=="standard":# right branch here
				if (ed=="Prof" or ed=="Doctorate"):
					if (gender=="male"):
						income="greater"
					else:
						income="less"
				else:
					income="less"
			else:
				if (ed=="Masters" or ed=="Prof" or ed=="Doctorate"):	##finish
					if (occ=="1"):
						income=="greater"
					else:
						income=="less"
				else:
					income="less"
					
				
	
	
	
#print(str(age))
#print(MS)
#print(ed)
#print(hours)
#print(income)

print("\nExpected annual income is "+income+" than $50,000") 
