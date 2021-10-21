from django.db import models

class Post(models.Model):
    title = models.TextField()
    cover = models.ImageField(upload_to='images/')
    uploaded_on = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return self.title