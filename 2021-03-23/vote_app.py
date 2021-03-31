from flask import Flask
from flask_restful import Api
from routes.get_description import GetDescription

def create_app():       
    app = Flask(__name__)
    api = Api(app)

    @app.route('/')
    def index():
        return 'Main page'

    api.add_resource(GetDescription, '/api/<string:rcid>')
    return app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True)
