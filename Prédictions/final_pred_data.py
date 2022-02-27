from download_data import data_base
from predict_d import prediction
import csv
import pandas as pd
import tensorflow as tf
from parametre import params

def pred_for_each_data():
    """Cette fonction prédit le montant, à la cloture, de chaque action pour chaque jour pendant 5 jours.
    liste_der_val est une liste contenant la dernière valeur (dernière cloture) de chaque action.
    Permet par la suite de comparer a j+5 le cour de chaque action (Hausse ou baisse).
    liste_pred retourne une liste de liste de chaque action, contenant les 5 valeurs prédites.
    """
    liste_valeur_action = list()
    with open("df_temp.csv","r") as csvfile:
      reader = csv.reader(csvfile)
      next(reader)
      for elem in reader:
        liste_valeur_action.append(int(elem[1]))

    liste_pred = list()
    liste_der_val = list()

    for i in range(len(liste_valeur_action)):

        data_autre = data_base()[i]
        liste_pred.append(prediction(data_autre)[0])
        liste_der_val.append(prediction(data_autre)[1])

    d = pd.DataFrame(liste_pred)
    d.to_csv("liste_pred.csv",index=False)
    dd = pd.DataFrame(liste_der_val)
    dd.to_csv("liste_der_val.csv",index=False)


def mean_value():
    """Cette focntion fait la somme de la prédiction de chaque action par jour prédit. Retourne une liste de 5 nombres représentant le montant total du portefeuille par jour.
    Exemple : 1er nombre de la liste evolution_port-> pred_1er_jour_action1*Nbre_action1 + pred_1er_jour_action2*Nbre_action2 + pred_1er_jour_action3*Nbre_action3 + etc...
    """
    liste_valeur_action = list()
    with open("df_temp.csv","r") as csvfile:
      reader = csv.reader(csvfile)
      next(reader)
      for elem in reader:
        liste_valeur_action.append(int(elem[1]))
    
    liste_pred = list()
    with open("liste_pred.csv","r") as csvfile:
      reader = csv.reader(csvfile)
      next(reader)
      for elem in reader:
        liste_pred.append(elem)
    
    liste_der_val = list()
    with open("liste_der_val.csv","r") as csvfile:
      reader = csv.reader(csvfile)
      next(reader)
      for elem in reader:
        liste_der_val.append(elem)

    # liste_pred = pred_for_each_data()[0]
    for i in range(len(liste_pred)):
        liste_pred[i] = [float(elem) * liste_valeur_action[i] for elem in liste_pred[i]]
    evolution_port = list(tf.reduce_sum(liste_pred, axis=0).numpy())
    dd = pd.DataFrame(evolution_port)
    dd.to_csv("evolution_port.csv",index=False)
    
    # return evolution_port
pred_for_each_data()
mean_value()