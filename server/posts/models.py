from django.db import models

# TODO Rename uploaded file before saving
# See https://docs.djangoproject.com/en/3.2/ref/models/fields/#django.db.models.FileField.upload_to
# def user_directory_path(instance, filename):
#     # file will be uploaded to MEDIA_ROOT/user_<id>/<filename>
#     return 'user_{0}/{1}'.format(instance.user.id, filename)
    
class Post(models.Model):
    title = models.TextField()
    cover = models.ImageField(upload_to='images/')
    uploaded_on = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return self.title