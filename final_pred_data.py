from download_data import data_base
from predict_d import prediction


def pred_for_each_data(liste_valeur_action):
    """Cette fonction prédit le montant, à la cloture, de chaque action pour chaque jour pendant 5 jours.
    liste_der_val est une liste contenant la dernière valeur (dernière cloture) de chaque action.
    Permet par la suite de comparer a j+5 le cour de chaque action (Hausse ou baisse).
    liste_pred retourne une liste de liste de chaque action, contenant les 5 valeurs prédites.
    """
    liste_pred = list()
    liste_der_val = list()

    for i in range(len(liste_valeur_action)):

        data_autre = data_base()[i]
        liste_pred.append(prediction(data_autre)[0])
        liste_der_val.append(prediction(data_autre)[1])

    return liste_pred, liste_der_val


def mean_value(liste_valeur_action):
    """Cette focntion fait la somme de la prédiction de chaque action par jour prédit. Retourne une liste de 5 nombres représentant le montant total du portefeuille par jour.
    Exemple : 1er nombre de la liste evolution_port-> pred_1er_jour_action1*Nbre_action1 + pred_1er_jour_action2*Nbre_action2 + pred_1er_jour_action3*Nbre_action3 + etc...
    """
    liste_pred = pred_for_each_data()[0]
    for i in range(len(liste_pred)):
        liste_pred[i] = [elem * liste_valeur_action[i] for elem in liste_pred[i]]
    evolution_port = list(tf.reduce_sum(liste_pred, axis=0).numpy())
    return evolution_port
