//Alim Yanis && Mellal Hodaifa

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

typedef  struct Element {
	char* Nom;
	int Type;
	int Nature;	
	struct Element* Suivant;
			} Element;

void InsertElt(Element** ListElts,char* Nom, int Type, int nat)	{

Element* nouveau = malloc(sizeof(Element));
Element* tete = *ListElts;
Element* prec;
nouveau->Nom=strdup(Nom);
nouveau->Type = Type;
nouveau->Nature = nat;
nouveau->Suivant = NULL;

if (tete == NULL ) *ListElts = nouveau ;
else {	while (tete != NULL) {
	prec = tete;
	tete = tete->Suivant;
				}
	prec->Suivant = nouveau;

      }
						}

int RechercheElt(Element* ListElts , char* Nom)	{

Element* tete = ListElts;

		while(tete != NULL)	{
		if (!strcmp(tete->Nom,Nom)) return 1;
		else tete = tete->Suivant;
					}
		return 0;
	
							}

int RechercheNat(Element* ListElts , char* Nom)	{

Element* tete = ListElts;

		while(tete != NULL)	{
		if (!strcmp(tete->Nom,Nom)) return tete->Nature;
		else tete = tete->Suivant;
					}
		return -1;
						}

int RechercheType(Element* ListElts , char* Nom)	{

Element* tete = ListElts;

		while(tete != NULL)	{
		if (!strcmp(tete->Nom,Nom)) return tete->Type;
		else tete = tete->Suivant;
					}
		return -1;
							}

void AffichageElts(Element* ListElts) 		{

int i=1;
char *Type , *Nature;
Element* tete = ListElts;

	if (tete == NULL) printf("La liste est vide .\n");
	else {  while(tete != NULL)     {
		if (tete->Type == 0 ) Type=strdup("Ufloat");
		else Type=strdup("Uint");
		if (tete->Nature == 0 ) Nature=strdup("Constant");
		else Nature=strdup("Variable");
		printf("\nElement %d : \n\nNom : %s\nType : %s\nNature : %s\n",i,tete->Nom,Type,Nature);
		i++;
		tete = tete->Suivant ;	}
	     }
						}



