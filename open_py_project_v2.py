import os
import requests
from pathlib import Path
from moviepy import (
    ImageClip,
    AudioFileClip,
    concatenate_videoclips
)

dossier_images = "images_picsum"
fichier_audio = "jazz_d_ascenseur.mp3"
fichier_sortie = Path.home() / "Downloads" / "open_py_diaporama.mp4"
duree_image = 3

def preparer_dossier(dossier):
    if os.path.exists(dossier):
        for fichier in os.listdir(dossier):
            if fichier.endswith(".jpg"):
                chemin_fichier = os.path.join(dossier, fichier)
                os.remove(chemin_fichier)
    else:
        os.makedirs(dossier)

def telecharger_images(nombre_images, dossier_images):
    for i in range(1, nombre_images + 1):
        nom = "image_{}.jpg".format(i)
        chemin = os.path.join(dossier_images, nom)

        try:
            url = "https://picsum.photos/800/600?random={}".format(i)
            reponse = requests.get(url)

            if reponse.status_code == 200:
                with open(chemin, "wb") as fichier:
                    fichier.write(reponse.content)
                print("Téléchargée: {}".format(nom))
            else:
                print("Erreur {} pour l'image {}".format(reponse.status_code, i))
        
        except Exception as e:
            print("Exception pour l'image {}: {}".format(i, e))

try:
    N = int(input("Nombre d'images à télécharger : "))
except ValueError:
    print("Entrée invalide, 5 images utilisées")
    N = 5

preparer_dossier(dossier_images)
telecharger_images(N, dossier_images)

liste_images = []
for image in sorted(os.listdir(dossier_images)):
    if image.endswith(".jpg"):
        chemin = os.path.join(dossier_images, image)
        liste_images.append(chemin)

clips = []
for image in liste_images:
    clip = ImageClip(image).with_duration(duree_image)
    clips.append(clip)

video = concatenate_videoclips(clips, method="compose")
audio = AudioFileClip(fichier_audio)
duree_video = video.duration
nombre_boucles = int(duree_video / audio.duration) + 1
audio_boucle = audio

for i in range(nombre_boucles - 1):
    from moviepy import concatenate_audioclips
    audio_boucle = concatenate_audioclips([audio_boucle, audio])

audio_final = audio_boucle.subclipped(0, duree_video)
video = video.with_audio(audio_final)

video.write_videofile(
    str(fichier_sortie),
    fps=24,
    codec="libx264",
    audio_codec="aac",
    logger=None
)
print("Diaporama généré :", fichier_sortie)